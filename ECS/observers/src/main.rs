use bevy::{
    platform::collections::{HashMap, HashSet},
    prelude::*,
};
use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .init_resource::<SpatialIndex>()
        .add_systems(Startup, setup)
        .add_systems(Update, (draw_shapes, handle_click))
        .add_observer(
            |trigger: Trigger<ExplodeMines>,
             mines: Query<&Mine>,
             index: Res<SpatialIndex>,
             mut commands: Commands| {
                let event = trigger.event();
                for e in index.get_nearby(event.pos) {
                    let mine = mines.get(e).unwrap();
                    if mine.pos.distance(event.pos) < mine.size + event.radius {
                        commands.trigger_targets(Explode, e);
                    }
                }
            },
        )
        .add_observer(on_add_mine)
        .add_observer(on_remove_mine)
        .run();
}

#[derive(Component)]
struct Mine {
    pos: Vec2,
    size: f32,
}

impl Mine {
    fn random(rand: &mut ChaCha8Rng) -> Self {
        Mine {
            pos: Vec2::new(
                (rand.random::<f32>() - 0.5) * 1200.0,
                (rand.random::<f32>() - 0.5) * 600.0,
            ),
            size: 4.0 + rand.random::<f32>() * 16.0,
        }
    }
}

#[derive(Event)]
struct ExplodeMines {
    pos: Vec2,
    radius: f32,
}

#[derive(Event)]
struct Explode;

fn setup(mut commands: Commands) {
    commands.spawn(Camera2d);
    commands.spawn((
        Text::new(
            "Click on a \"Mine\" to trigger it.\n\
            When it explodes it will trigger all overlapping mines.",
        ),
        Node {
            position_type: PositionType::Absolute,
            top: Val::Px(12.),
            left: Val::Px(12.),
            ..default()
        },
    ));

    let mut rng = ChaCha8Rng::seed_from_u64(19878367467713);

    commands.spawn(Mine::random(&mut rng)).observe(explode_mine);

    let mut observer = Observer::new(explode_mine);

    for _ in 0..1000 {
        let entity = commands.spawn(Mine::random(&mut rng)).id();
        observer.watch_entity(entity);
    }

    commands.spawn(observer);
}

fn on_add_mine(
    trigger: Trigger<OnAdd, Mine>,
    query: Query<&Mine>,
    mut index: ResMut<SpatialIndex>,
) {
    let mine = query.get(trigger.target()).unwrap();
    let tile = (
        (mine.pos.x / CELL_SIZE).floor() as i32,
        (mine.pos.y / CELL_SIZE).floor() as i32,
    );
    index.map.entry(tile).or_default().insert(trigger.target());
}

fn on_remove_mine(
    trigger: Trigger<OnRemove, Mine>,
    query: Query<&Mine>,
    mut index: ResMut<SpatialIndex>,
) {
    let mine = query.get(trigger.target()).unwrap();
    let tile = (
        (mine.pos.x / CELL_SIZE).floor() as i32,
        (mine.pos.y / CELL_SIZE).floor() as i32,
    );
    index.map.entry(tile).and_modify(|set| {
        set.remove(&trigger.target());
    });
}

fn explode_mine(trigger: Trigger<Explode>, query: Query<&Mine>, mut commands: Commands) {
    let id = trigger.target();
    let Ok(mut entity) = commands.get_entity(id) else {
        return;
    };
    info!("Boom! {} exploded.", id.index());
    entity.despawn();
    let mine = query.get(id).unwrap();
    commands.trigger(ExplodeMines {
        pos: mine.pos,
        radius: mine.size,
    });
}

fn draw_shapes(mut gizmos: Gizmos, mines: Query<&Mine>) {
    for mine in &mines {
        gizmos.circle_2d(
            mine.pos,
            mine.size,
            Color::hsl((mine.size - 4.0) / 16.0 * 360.0, 1.0, 0.8),
        );
    }
}

fn handle_click(
    mouse_button_input: Res<ButtonInput<MouseButton>>,
    camera: Single<(&Camera, &GlobalTransform)>,
    windows: Query<&Window>,
    mut commands: Commands,
) {
    let Ok(windows) = windows.single() else {
        return;
    };

    let (camera, camera_transform) = *camera;
    if let Some(pos) = windows
        .cursor_position()
        .and_then(|cursor| camera.viewport_to_world(camera_transform, cursor).ok())
        .map(|ray| ray.origin.truncate())
    {
        if mouse_button_input.just_pressed(MouseButton::Left) {
            commands.trigger(ExplodeMines { pos, radius: 1.0 });
        }
    }
}

#[derive(Resource, Default)]
struct SpatialIndex {
    map: HashMap<(i32, i32), HashSet<Entity>>,
}

const CELL_SIZE: f32 = 64.0;

impl SpatialIndex {
    fn get_nearby(&self, pos: Vec2) -> Vec<Entity> {
        let tile = (
            (pos.x / CELL_SIZE).floor() as i32,
            (pos.y / CELL_SIZE).floor() as i32,
        );
        let mut nearby = Vec::new();
        for x in -1..2 {
            for y in -1..2 {
                if let Some(mines) = self.map.get(&(tile.0 + x, tile.1 + y)) {
                    nearby.extend(mines.iter());
                }
            }
        }
        nearby
    }
}
