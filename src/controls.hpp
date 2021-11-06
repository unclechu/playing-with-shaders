#pragma once

#include <vector>
#include <functional>
#include <variant>

#include <GLFW/glfw3.h>


enum ZoomState {
  Standby,
  ZoomIn,
  ZoomOut,
};

struct MouseControls {
  int mouse_pos_x = 0;
  int mouse_pos_y = 0;
  bool left_button_pressed = false;
};

struct Controls {
  const std::vector<int> fast_keys { GLFW_KEY_LEFT_SHIFT, GLFW_KEY_RIGHT_SHIFT };
  const std::vector<int> slow_keys { GLFW_KEY_LEFT_ALT, GLFW_KEY_RIGHT_ALT };

  unsigned int fast_pressed_counter = 0; // thread-unsafe (only for the key event callback)
  unsigned int slow_pressed_counter = 0; // thread-unsafe (only for the key event callback)
  bool fast_pressed = false;
  bool slow_pressed = false;

  const double move_step_norm = 1.0;
  const double move_step_fast = 10.0;
  const double move_step_slow = 0.1;

  double move_step = move_step_norm;

  const int move_left_key = GLFW_KEY_LEFT;
  bool move_left_pressed = false;

  const int move_right_key = GLFW_KEY_RIGHT;
  bool move_right_pressed = false;

  const int move_up_key = GLFW_KEY_UP;
  bool move_up_pressed = false;

  const int move_down_key = GLFW_KEY_DOWN;
  bool move_down_pressed = false;

  const double zoom_step_norm = 1.0;
  const double zoom_step_fast = 10.0;
  const double zoom_step_slow = 0.1;

  double zoom_step = zoom_step_norm;

  const int zoom_in_key = GLFW_KEY_Z;
  const int zoom_out_key = GLFW_KEY_X;

  bool zoom_in_pressed = false;
  bool zoom_out_pressed = false;
  ZoomState zoom_state = Standby;

  const int reset_key = GLFW_KEY_R;
  bool reset_pressed = false;

  MouseControls mouse;
};

void on_key_event_controls_cb(
  Controls *controls,
  int key,
  int scancode,
  int action,
  int mods
);

struct MousePosEvent {
  int xpos;
  int ypos;
};

struct MouseScrollEvent {
  int xoffset;
  int yoffset;
};

struct MouseButtonEvent {
  int button;
  int action;
  int mods;
};

void on_mouse_event_controls_cb(
  MouseControls *controls,

  std::variant<
    MousePosEvent,
    MouseScrollEvent,
    MouseButtonEvent
  > mouse_event
);
