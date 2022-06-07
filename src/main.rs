#![feature(drain_filter)]

use std::cell::{RefCell, RefMut, Ref};
use std::collections::HashMap;
use std::ops::{AddAssign, SubAssign, Mul, Add, Div};
use wbg_rand::{wasm_rng, Rng};

use buttons::*;
use speedy2d::color::Color;
use speedy2d::dimen::{Vec2, Vector2, UVec2};
use speedy2d::font::Font;
use speedy2d::window::{
    KeyScancode,
    MouseButton,
    VirtualKeyCode,
    WindowFullscreenMode,
    WindowHandler,
    WindowHelper,
    WindowStartupInfo
};
use speedy2d::{Graphics2D, WebCanvas};

#[cfg(not(target_arch = "wasm32"))]
compile_error!("This sample only builds for WebAssembly (wasm32)");

mod buttons;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
enum UserEvent
{
    ButtonClickEnableFullscreen,
    ButtonClickResetPositions,
    ButtonClickRandomizePositions,
    ButtonClickIncreaseSpeed,
    ButtonClickDecreaseSpeed,
    ButtonClickResetOrigin,
    ButtonClickTerminate,
}

#[derive(Clone, Debug, PartialEq)]
struct PV(f64, f64);

impl PV {
    #[inline]
    pub fn x(&self) -> f64 {
        self.0
    }

    #[inline]
    pub fn y(&self) -> f64 {
        self.1
    }
}

impl Into<Vec2> for PV {
    fn into(self) -> Vec2 {
        Vector2::<f32>::new(self.0 as f32, self.1 as f32)
    }
}

impl AddAssign<(f64, f64)> for PV {
    fn add_assign(&mut self, rhs: (f64, f64)) {
        self.0 += rhs.0;
        self.1 += rhs.1;
    }
}

impl SubAssign<(f64, f64)> for PV {
    fn sub_assign(&mut self, rhs: (f64, f64)) {
        self.0 -= rhs.0;
        self.1 -= rhs.1;
    }
}

impl<T> Add<T> for PV 
where
    T: Into<PV>,
{
    type Output = PV;
    fn add(self, rhs: T) -> Self::Output {
        let p = rhs.into();
        PV(self.0 + p.0, self.1 + p.1)
    }
}

impl Mul<f64> for PV {
    type Output = PV;
    fn mul(self, rhs: f64) -> Self::Output {
        PV(self.0 * rhs, self.1 * rhs)
    }
}

impl Div<f64> for PV {
    type Output = PV;
    fn div(self, rhs: f64) -> Self::Output {
        PV(self.0 / rhs, self.1 / rhs)
    }
}

impl From<PV> for (f64, f64) {
    fn from(t: PV) -> Self {
        (t.0, t.1)
    }
}

#[derive(Clone, Debug, PartialEq)]
struct MassedCircle {
    id: u32,
    mass: f64,
    radius: f64,
    pos: PV,
    vel: PV,
}

struct Masses(Vec<RefCell<MassedCircle>>, u32);

const USE_DISTANCE_CHECK: bool = true;
const GRAVITY: f64 = 6673.85 / 4.0; 

impl Masses {
    pub fn new() -> Self {
        Self(Vec::new(), 0)
    }

    fn start_id_cnt(&mut self, id: u32) {
        self.1 = id;
    }

    fn id(&mut self) -> u32 {
        self.1 += 1;
        self.1
    }

    pub fn reset(&mut self, dim: UVec2) {
        self.0.clear();

        masses!(300, 0.125, 3, self.0, (dim.x / 2), (dim.y / 2));

        self.start_id_cnt(self.0.len() as u32 + 1);
    }

    pub fn randomize(&mut self, dim: UVec2) {
        self.reset(dim);

        let mut rng = wasm_rng();

        for m in self.0.iter_mut() {
            let mut m = m.borrow_mut();
            m.pos.0 = rng.gen_range(0.0, 1.0) * dim.x as f64;
            m.pos.1 = rng.gen_range(0.0, 1.0) * dim.y as f64;
        }
    }

    pub fn find(&self, id: u32) -> Option<&RefCell<MassedCircle>> {
        Masses::find_in(id, &self.0)
    }

    pub fn find_in<'a>(id: u32, v: &'a Vec<RefCell<MassedCircle>>) -> Option<&'a RefCell<MassedCircle>> {
        for m in v.iter() {
            let rm = m.borrow();
            if rm.id == id {
                return Some(m);
            }
        }

        None
    }

    pub fn findm<'a>(&'a self, id: u32, merges: &HashMap<u32, u32>, new: &'a Vec<RefCell<MassedCircle>>) -> &'a RefCell<MassedCircle> {
        let mut id = id;
        let mut use_new = false;
        while let Some(nid) = merges.get(&id) {
            id = *nid;
            use_new = true;
        }

        if use_new {
            Masses::find_in(id, new).unwrap()
        } else {
            self.find(id).unwrap()
        }

    }

    fn check_collision(m1: RefMut<MassedCircle>, m2: RefMut<MassedCircle>, d: f64, res: &mut Vec<(u32, u32)>) {
        if d < (m1.radius + m2.radius) * 0.85 {
            if {
                let mut found = false;
                for c in res.iter() {
                    if c.0 == m1.id || c.1 == m1.id || c.0 == m2.id || c.1 == m2.id {
                        found = true;
                    }
                }
                !found
            } {
                res.push((m1.id, m2.id));
            }
        }
    }

    pub fn update(&mut self, time_step: f64) { // This is an O(n^2) operation, be careful with the len
        let mut collisions = Vec::new();
        for x in 0..self.0.len() {
            for y in 0..self.0.len() {
                if x == y {
                    continue;
                }

                let mut m1 = self.0.get(x).unwrap().borrow_mut();
                let mut m2 = self.0.get(y).unwrap().borrow_mut();

                let dx = m2.pos.x() - m1.pos.x();
                let dy = m2.pos.y() - m1.pos.y();
                let d = f64::sqrt(dx*dx + dy*dy);

                if USE_DISTANCE_CHECK && (d < 0.6 || d > 300.0) {
                    continue;
                }

                let at = GRAVITY * time_step * d.powi(-3);
                let ax = at * dx;
                let ay = at * dy;

                let f1 = (ax * m2.mass, ay * m2.mass);
                let f2 = (ax * m1.mass, ay * m1.mass);

                m1.vel += f1;
                m2.vel -= f2;

                Masses::check_collision(m1, m2, d, &mut collisions);
            }
        }

        for b in self.0.iter_mut() {
            let mut b = b.borrow_mut();
            let dp = b.vel.clone() * time_step;
            b.pos += dp.into();
        }

        let mut merge_map = HashMap::<u32, u32>::new();
        let mut new_bodies = Vec::<RefCell<MassedCircle>>::new();

        for c in collisions.iter() {
            let id = self.id();
            let search = new_bodies.clone();
            let m1 = self.findm(c.0, &merge_map, &search).borrow();

            let m2 = self.findm(c.1, &merge_map, &search).borrow();

            let mass = m1.mass + m2.mass;
            let radius = (m1.radius.powi(3) + m2.radius.powi(3)).cbrt();
            let vel = (m1.vel.clone() * m1.mass + m2.vel.clone() * m2.mass) / mass;
            let pos = (m1.pos.clone() * m1.mass + m2.pos.clone() * m2.mass) / mass;

            let nm = RefCell::new(MassedCircle {
                id,
                mass,
                radius,
                vel,
                pos
            });

            merge_map.insert(m1.id, id);
            merge_map.insert(m2.id, id);

            new_bodies.push(nm);

        }

        self.0.drain_filter(|c| {
            let c: Ref<MassedCircle> = (*c).borrow();
            merge_map.clone().into_iter().filter(|e| e.0 == c.id).count() > 0
        });

        for b in new_bodies {
            self.0.push(b);
        }
    }
}

struct MyHandler
{
    font: Font,
    buttons: ButtonGroup<UserEvent>,
    scale: f32,
    masses: Masses,
    time_step: f64,
    dim: UVec2,
    offset: PV,
    up: bool,
    down: bool,
    left: bool,
    right: bool,
}

const OFFSET_MOD: f64 = 4.0;

impl WindowHandler<UserEvent> for MyHandler
{
    fn on_start(&mut self, helper: &mut WindowHelper<UserEvent>, info: WindowStartupInfo)
    {
        helper.set_title("three-body sim");

        self.scale = info.scale_factor() as f32;
        self.dim = *info.viewport_size_pixels();
        self.masses.randomize(self.dim);

        let event_sender = helper.create_user_event_sender();

        self.buttons.add(Button::new(
            "Enable fullscreen",
            self.font.clone(),
            TriggerableEvent::new(&event_sender, UserEvent::ButtonClickEnableFullscreen)
        ));

        self.buttons.add(Button::new(
            "Reset positions",
            self.font.clone(),
            TriggerableEvent::new(&event_sender, UserEvent::ButtonClickResetPositions)
        ));

        self.buttons.add(Button::new(
            "Randomize positions",
            self.font.clone(),
            TriggerableEvent::new(&event_sender, UserEvent::ButtonClickRandomizePositions)
        ));

        self.buttons.add(Button::new(
            "Increase speed",
            self.font.clone(),
            TriggerableEvent::new(&event_sender, UserEvent::ButtonClickIncreaseSpeed)
        ));

        self.buttons.add(Button::new(
            "Decrease speed",
            self.font.clone(),
            TriggerableEvent::new(&event_sender, UserEvent::ButtonClickDecreaseSpeed)
        ));

        self.buttons.add(Button::new(
            "Reset origin",
            self.font.clone(),
            TriggerableEvent::new(&event_sender, UserEvent::ButtonClickResetOrigin)
        ));

        self.buttons.add(Button::new(
            "Terminate",
            self.font.clone(),
            TriggerableEvent::new(&event_sender, UserEvent::ButtonClickTerminate)
        ));
    }

    fn on_user_event(
        &mut self,
        helper: &mut WindowHelper<UserEvent>,
        user_event: UserEvent
    )
    {
        match user_event {
            UserEvent::ButtonClickEnableFullscreen => {
                helper.set_fullscreen_mode(WindowFullscreenMode::FullscreenBorderless)
            },
            UserEvent::ButtonClickResetPositions => {
                self.masses.reset(self.dim);
            },
            UserEvent::ButtonClickRandomizePositions => {
                self.masses.randomize(self.dim);
            },
            UserEvent::ButtonClickIncreaseSpeed => {
                self.time_step *= 2.0;
            },
            UserEvent::ButtonClickDecreaseSpeed => {
                if self.time_step >= 0.0625 {
                    self.time_step /= 2.0;
                }
            },
            UserEvent::ButtonClickResetOrigin => {
                self.offset = PV(0.0, 0.0);
            }
            UserEvent::ButtonClickTerminate => helper.terminate_loop(),
        }
    }

    fn on_resize(&mut self, _: &mut WindowHelper<UserEvent>, size_pixels: speedy2d::dimen::UVec2) {
        self.dim = size_pixels;
    }

    fn on_scale_factor_changed(
        &mut self,
        _helper: &mut WindowHelper<UserEvent>,
        scale_factor: f64
    )
    {
        self.scale = scale_factor as f32;
    }

    fn on_draw(&mut self, helper: &mut WindowHelper<UserEvent>, graphics: &mut Graphics2D)
    {
        graphics.clear_screen(Color::from_gray(0.1));

        self.buttons
            .draw(graphics, Vec2::new(20.0, 20.0), self.scale);

        for c in self.masses.0.clone() {
            let c = c.borrow();
            graphics.draw_circle(c.pos.clone() + self.offset.clone(), c.radius as f32, Color::from_gray(0.7));
        }

        let mut dx = 0.0;
        let mut dy = 0.0;

        if self.left {
            dx += 1.0;
        }

        if self.right {
            dx -= 1.0;
        }

        if self.up {
            dy += 1.0;
        }

        if self.down {
            dy -= 1.0;
        }

        self.offset.0 += dx * OFFSET_MOD;
        self.offset.1 += dy * OFFSET_MOD;

        self.masses.update(self.time_step);

        // Request that we draw another frame once this one has finished
        helper.request_redraw();
    }

    fn on_key_down(
            &mut self,
            _: &mut WindowHelper<UserEvent>,
            virtual_key_code: Option<VirtualKeyCode>,
            _: KeyScancode
        ) {
        if let Some(code) = virtual_key_code {
            match code {
                VirtualKeyCode::Up => {
                    self.up = true;
                },
                VirtualKeyCode::Down => {
                    self.down = true;
                },
                VirtualKeyCode::Left => {
                    self.left = true;
                },
                VirtualKeyCode::Right => {
                    self.right = true;
                },
                _ => {}
            }
        }
    }

    fn on_key_up(
        &mut self,
        _: &mut WindowHelper<UserEvent>,
        virtual_key_code: Option<VirtualKeyCode>,
        _: KeyScancode
    ) {
        if let Some(code) = virtual_key_code {
            match code {
                VirtualKeyCode::Up => {
                    self.up = false;
                },
                VirtualKeyCode::Down => {
                    self.down = false;
                },
                VirtualKeyCode::Left => {
                    self.left = false;
                },
                VirtualKeyCode::Right => {
                    self.right = false;
                },
                _ => {}
            }
        }
    }

    fn on_mouse_move(&mut self, _helper: &mut WindowHelper<UserEvent>, position: Vec2)
    {
        self.buttons.on_mouse_move(position);
    }

    fn on_mouse_button_down(
        &mut self,
        _helper: &mut WindowHelper<UserEvent>,
        button: MouseButton
    )
    {
        if button == MouseButton::Left {
            self.buttons.on_mouse_left_down();
        }
    }

    fn on_mouse_button_up(
        &mut self,
        _helper: &mut WindowHelper<UserEvent>,
        button: MouseButton
    )
    {
        if button == MouseButton::Left {
            self.buttons.on_mouse_left_up();
        }
    }
}

fn main()
{
    wasm_logger::init(wasm_logger::Config::default());
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    log::info!("Speedy2D WebGL sample");

    let font =
        Font::new(include_bytes!("../NotoSans-Regular.ttf")).unwrap();

    WebCanvas::new_for_id_with_user_events(
        "my_canvas",
        MyHandler {
            font,
            buttons: ButtonGroup::new(),
            scale: 1.0,
            masses: Masses::new(),
            time_step: 0.25 / 2.0,
            dim: Vector2 { x: 0, y: 0 },
            offset: PV(0.0, 0.0),
            up: false,
            down: false,
            left: false,
            right: false,
        }
    )
    .unwrap();
}

#[macro_export]
macro_rules! mass {
    ($cnt:expr, $idx:expr, $mass:expr, $rad:expr, $cx:expr, $cy:expr) => {{
        let each = std::f64::consts::TAU / ($cnt as f64);
        RefCell::new(MassedCircle {
            id: $idx,
            mass: $mass as f64,
            radius: $rad as f64,
            pos: PV(
                {
                    $cx as f64 + (f64::cos(each * ($idx as f64)) * 250f64)
                },
                {
                    $cy as f64 + (f64::sin(each * ($idx as f64)) * 250f64)
                }
            ),
            vel: PV(0f64,0f64)
        })
    }};
}

#[macro_export]
macro_rules! masses {
    ($cnt:expr, $mass:expr, $rad:expr, $v:expr, $cx:expr, $cy:expr) => {{
        for i in 0..$cnt {
            $v.push(mass!($cnt, i, $mass, $rad, $cx, $cy));
        }
    }};
}