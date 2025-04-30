use std::f32::consts::{PI, SQRT_2};

use bevy::{
    color::palettes::css::{RED, WHITE},
    input::common_conditions::input_just_pressed,
    math::{
        bounding::{
            Aabb2d, Bounded2d, Bounded3d, BoundedExtrusion, BoundingCircle, BoundingVolume,
        },
        Isometry2d,
    },
    prelude::*,
    render::{
        camera::ScalingMode,
        mesh::{Extrudable, ExtrusionBuilder, PerimeterSegment},
        render_asset::RenderAssetUsages,
    },
};

const HEART: Heart = Heart::new(0.5);
const EXTRUSION: Extrusion<Heart> = Extrusion {
    base_shape: Heart::new(0.5),
    half_depth: 0.5,
};

const TRANSFORM_2D: Transform = Transform {
    translation: Vec3::ZERO,
    rotation: Quat::IDENTITY,
    scale: Vec3::ONE,
};
const PROJECTION_2D: Projection = Projection::Orthographic(OrthographicProjection { near: -1.0, far: 10.0, viewport_origin: Vec2::new(0.5,0.5), scaling_mode: ScalingMode::AutoMax { max_width: 8.0, max_height: 20.0 }, scale: 1.0, area: Rect { min: Vec2::NEG_ONE, max: Vec2::ONE } });

const TRANSFORM_3D: Transform = Transform {
    translation: Vec3::ZERO,
    rotation: Quat::from_xyzw(-0.14521316, -0.0, -0.0, 0.98940045),
    scale: Vec3::ONE,
};
const PROJECTION_3D: Projection = Projection::Perspective(PerspectiveProjection { fov: PI / 4.0, aspect_ratio: 1.0, near: 0.1, far: 1000.0 });

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, States, Default, Reflect)]
enum CameraActive {
    #[default]
    Dim2,
    Dim3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, States, Default, Reflect)]
enum BoundingShape {
    #[default]
    None,
    BoundingSphere,
    BoundingBox,
}

#[derive(Component)]
struct Shape2d;

#[derive(Component)]
struct Shape3d;

fn main() {
    App::new().add_plugins(DefaultPlugins).init_state::<BoundingShape>().init_state::<CameraActive>().add_systems(Startup, setup).add_systems(Update, (
        (rotate_2d_shapes, bounding_shapes_2d).run_if(in_state(CameraActive::Dim2)),
        (rotate_3d_shapes, bounding_shapes_3d).run_if(in_state(CameraActive::Dim3)),
        update_bounding_shape.run_if(input_just_pressed(KeyCode::KeyB)),
        switch_cameras.run_if(input_just_pressed(KeyCode::Space)),
    ),).run();
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,   
) {
    commands.spawn((Camera3d::default(), TRANSFORM_2D, PROJECTION_2D));

    commands.spawn((
        Mesh3d(meshes.add(HEART.mesh().resolution(50))),
        MeshMaterial3d(materials.add(StandardMaterial {
            emissive: RED.into(),
            base_color: RED.into(),
            ..Default::default()
        })),
        Transform::from_xyz(0.0, 0.0, 0.0),
        Shape2d,
    ));

    commands.spawn((
        Mesh3d(meshes.add(EXTRUSION.mesh().resolution(50))),
        MeshMaterial3d(materials.add(StandardMaterial {
            base_color: RED.into(),
            ..Default::default()
        })),
        Transform::from_xyz(0.0, -3.0, -10.0).with_rotation(Quat::from_rotation_x(-PI / 4.0)),
        Shape3d,
    ));

    commands.spawn((
        PointLight {
            shadows_enabled: true,
            intensity: 10_000_000.0,
            range: 100.0,
            shadow_depth_bias: 0.2,
            ..default()
        },
        Transform::from_xyz(8.0, 12.0, 1.0),
    ));

    commands.spawn((
        Text::new("Press 'B' to toggle between no bounding shapes, bounding boxes (AABBs) and bounding spheres / circles\n\
            Press 'Space' to switch between 3D and 2D"),
        Node {
            position_type: PositionType::Absolute,
            top: Val::Px(12.0),
            left: Val::Px(12.0),
            ..default()
        },
    ));
}

fn rotate_2d_shapes(mut shapes: Query<&mut Transform, With<Shape2d>>, time: Res<Time>) {
    let elapsed_seconds = time.elapsed_secs();

    for mut transform in shapes.iter_mut() {
        transform.rotation = Quat::from_rotation_z(elapsed_seconds);
    }
}

fn bounding_shapes_2d(
    shapes: Query<&Transform, With<Shape2d>>,
    mut gizmos: Gizmos,
    bounding_shape: Res<State<BoundingShape>>,
) {
    for transform in shapes.iter() {
        let rotation = transform.rotation.to_scaled_axis().z;
        let rotation = Rot2::radians(rotation);
        let isometry = Isometry2d::new(transform.translation.xy(), rotation);

        match bounding_shape.get() {
            BoundingShape::None => (),
            BoundingShape::BoundingBox => {
                let aabb = HEART.aabb_2d(isometry);
                gizmos.rect_2d(aabb.center(), aabb.half_size() * 2.0, WHITE);
            }
            BoundingShape::BoundingSphere => {
                let bounding_circle = HEART.bounding_circle(isometry);
                gizmos.circle_2d(bounding_circle.center(), bounding_circle.radius(), WHITE).resolution(64);
            }
        }
    }
}

fn rotate_3d_shapes(mut shapes: Query<&mut Transform, With<Shape3d>>, time: Res<Time>) {
    let delta_seconds = time.delta_secs();

    for mut transform in shapes.iter_mut() {
        transform.rotate_y(delta_seconds);
    }
}

fn bounding_shapes_3d(shapes: Query<&Transform, With<Shape3d>>, mut gizmos: Gizmos, bounding_shape: Res<State<BoundingShape>>,) {
    for transform in shapes.iter() {
        match bounding_shape.get() {
            BoundingShape::None => (),
            BoundingShape::BoundingBox => {
                let aabb = EXTRUSION.aabb_3d(transform.to_isometry());

                gizmos.primitive_3d(
                    &Cuboid::from_size(Vec3::from(aabb.half_size()) * 2.),
                    aabb.center(),
                    WHITE,
                );
            }
            BoundingShape::BoundingSphere => {
                let bounding_sphere = EXTRUSION.bounding_sphere(transform.to_isometry());
                gizmos.sphere(bounding_sphere.center(), bounding_sphere.radius(), WHITE);
            }
        }
    }
}

fn update_bounding_shape(
    current: Res<State<BoundingShape>>,
    mut next: ResMut<NextState<BoundingShape>>,
) {
    next.set(match current.get() {
        BoundingShape::None => BoundingShape::BoundingBox,
        BoundingShape::BoundingBox => BoundingShape::BoundingSphere,
        BoundingShape::BoundingSphere => BoundingShape::None,
    });
}

fn switch_cameras(
    current: Res<State<CameraActive>>,
    mut next: ResMut<NextState<CameraActive>>,
    camera: Single<(&mut Transform, &mut Projection)>,
) {
    let next_state = match current.get() {
        CameraActive::Dim2 => CameraActive::Dim3,
        CameraActive::Dim3 => CameraActive::Dim2,
    };
    next.set(next_state);

    let (mut transform, mut projection) = camera.into_inner();
    match next_state {
        CameraActive::Dim2 => {
            *transform = TRANSFORM_2D;
            *projection = PROJECTION_2D;
        }
        CameraActive::Dim3 => {
            *transform = TRANSFORM_3D;
            *projection = PROJECTION_3D;
        }
    };
}

#[derive(Copy, Clone)]
struct Heart {
    radius: f32,
}

impl Primitive2d for Heart {}

impl Heart {
    const fn new(radius: f32) -> Self {
        Self { radius }
    }
}

impl Measured2d for Heart {
    fn perimeter(&self) -> f32 {
        self.radius * (2.5 * PI + ops::powf(2f32, 1.5) + 2.0)
    }

    fn area(&self) -> f32 {
        let circle_area = PI * self.radius * self.radius;
        let triangle_area = self.radius * self.radius * (1.0 + 2f32.sqrt()) / 2.0;
        let cutout = triangle_area - circle_area * 3.0 / 16.0;
        
        2.0 * circle_area + 4.0 * cutout
    }
}

impl Bounded2d for Heart {
    fn aabb_2d(&self, isometry: impl Into<Isometry2d>) -> Aabb2d {
        let isometry = isometry.into();

        let circle_center = isometry.rotation * Vec2::new(self.radius, 0.0);
        let max_circle = circle_center.abs() + Vec2::splat(self.radius);
        let min_circle = -max_circle;

        let tip_position = isometry.rotation * Vec2::new(0.0, -self.radius * (1.0 + SQRT_2));

        Aabb2d { 
            min: isometry.translation + min_circle.min(tip_position),
            max: isometry.translation + max_circle.max(tip_position), 
        }
    }

    fn bounding_circle(&self, isometry: impl Into<Isometry2d>) -> BoundingCircle {
        let isometry = isometry.into();

        let offset = self.radius / ops::powf(2f32, 1.5);
        let center = isometry * Vec2::new(0.0, -offset);
        let radius = self.radius * (1.0 + 2f32.sqrt()) - offset;

        BoundingCircle::new(center, radius)
    }
}

impl BoundedExtrusion for Heart {}

impl Meshable for Heart {
    type Output = HeartMeshBuilder;

    fn mesh(&self) -> Self::Output {
        Self::Output {
            heart: *self,
            resolution: 32,
        }
    }
}

struct HeartMeshBuilder {
    heart: Heart,
    resolution: usize,
}

trait HeartBuilder {
    fn resolution(self, resolution: usize) -> Self;
}

impl HeartBuilder for HeartMeshBuilder {
    fn resolution(mut self, resolution: usize) -> Self {
        self.resolution = resolution;
        self
    }
}

impl HeartBuilder for ExtrusionBuilder<Heart> {
    fn resolution(mut self, resolution: usize) -> Self {
        self.base_builder.resolution = resolution;
        self
    }
}

impl MeshBuilder for HeartMeshBuilder {
    fn build(&self) -> Mesh {
        let radius = self.heart.radius;
        let wing_angle = PI * 1.25;

        let mut vertices = Vec::with_capacity(2 * self.resolution);
        let mut uvs = Vec::with_capacity(2 * self.resolution);
        let mut indices = Vec::with_capacity(6 * self.resolution - 9);
        let normals = vec![[0f32, 0f32, 1f32]; 2 * self.resolution];

        vertices.push([0.0; 3]);
        uvs.push([0.5, 0.5]);

        for i in 1..self.resolution {
            let angle = (i as f32 / self.resolution as f32) * wing_angle;
            let (sin, cos) = ops::sin_cos(angle);
            vertices.push([radius * (cos - 1.0), radius * sin, 0.0]);
            uvs.push([0.5 - (cos - 1.0) / 4., 0.5 - sin / 2.]);
        }

        vertices.push([0.0, radius * (-1. - SQRT_2), 0.0]);
        uvs.push([0.5, 1.]);

        for i in 0..self.resolution - 1 {
            let angle = (i as f32 / self.resolution as f32) * wing_angle - PI / 4.;
            let (sin, cos) = ops::sin_cos(angle);
            vertices.push([radius * (cos + 1.0), radius * sin, 0.0]);
            uvs.push([0.5 - (cos + 1.0) / 4., 0.5 - sin / 2.]);
        }

        for i in 2..2 * self.resolution as u32 {
            indices.extend_from_slice(&[i - 1, i, 0]);
        }

        Mesh::new(
            bevy::render::mesh::PrimitiveTopology::TriangleList,
            RenderAssetUsages::default(),
        )
        .with_inserted_indices(bevy::render::mesh::Indices::U32(indices))
        .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, vertices)
        .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
        .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
    }
}

impl Extrudable for HeartMeshBuilder {
    fn perimeter(&self) -> Vec<PerimeterSegment> {
        let resolution = self.resolution as u32;
        vec![
            PerimeterSegment::Smooth { 
                first_normal: Vec2::X, 
                last_normal: Vec2::new(-1.0, -1.0).normalize(), 
                indices: (0..resolution).collect() 
            },
            PerimeterSegment::Flat {
                indices: vec![resolution - 1, resolution, resolution + 1],
            },
            PerimeterSegment::Smooth {
                first_normal: Vec2::new(1.0, -1.0).normalize(),
                last_normal: Vec2::NEG_X,
                indices: (resolution + 1..2 * resolution).chain([0]).collect(),
            },
        ]
    }
}
