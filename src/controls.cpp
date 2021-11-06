#include <iostream> // just for debugging
#include <variant>

#include "controls.hpp"
#include "helpers.hpp"

using namespace std;


void on_key_event_controls_cb(
  Controls *controls,
  int key,
  int scancode,
  int action,
  int mods
)
{
  if (key == controls->reset_key) {
    if (action == GLFW_PRESS)
      controls->reset_pressed = true;
    else if (action == GLFW_RELEASE)
      controls->reset_pressed = false;
  }
  else if (key == controls->move_left_key)
  {
    if (action == GLFW_PRESS)
      controls->move_left_pressed = true;
    else if (action == GLFW_RELEASE)
      controls->move_left_pressed = false;
  }
  else if (key == controls->move_right_key)
  {
    if (action == GLFW_PRESS)
      controls->move_right_pressed = true;
    else if (action == GLFW_RELEASE)
      controls->move_right_pressed = false;
  }
  else if (key == controls->move_up_key)
  {
    if (action == GLFW_PRESS)
      controls->move_up_pressed = true;
    else if (action == GLFW_RELEASE)
      controls->move_up_pressed = false;
  }
  else if (key == controls->move_down_key)
  {
    if (action == GLFW_PRESS)
      controls->move_down_pressed = true;
    else if (action == GLFW_RELEASE)
      controls->move_down_pressed = false;
  }

  else if (key == controls->zoom_in_key || key == controls->zoom_out_key)
  {
    if (action == GLFW_PRESS) {
      if (key == controls->zoom_in_key)
        controls->zoom_in_pressed = true;
      else
        controls->zoom_out_pressed = true;
    } else if (action == GLFW_RELEASE) {
      if (key == controls->zoom_in_key)
        controls->zoom_in_pressed = false;
      else
        controls->zoom_out_pressed = false;
    }

    if (controls->zoom_in_pressed && !controls->zoom_out_pressed) {
      controls->zoom_state = ZoomIn;
    } else if (!controls->zoom_in_pressed && controls->zoom_out_pressed) {
      controls->zoom_state = ZoomOut;
    } else {
      controls->zoom_state = Standby;
    }
  }

  else if (
    find(controls->fast_keys.begin(), controls->fast_keys.end(), key)
      != controls->fast_keys.end()
  )
  {
    if (action == GLFW_PRESS)
      ++controls->fast_pressed_counter;
    else if (action == GLFW_RELEASE)
      --controls->fast_pressed_counter;

    // Just in case, if some events are not delivered or something
    if (controls->fast_pressed_counter > controls->fast_keys.size())
      controls->fast_pressed_counter = 0;

    controls->fast_pressed = controls->fast_pressed_counter > 0;

    if (controls->fast_pressed && !controls->slow_pressed) {
      controls->move_step = controls->move_step_fast;
      controls->zoom_step = controls->zoom_step_fast;
    } else if (!controls->fast_pressed && controls->slow_pressed) {
      controls->move_step = controls->move_step_slow;
      controls->zoom_step = controls->zoom_step_slow;
    } else {
      controls->move_step = controls->move_step_norm;
      controls->zoom_step = controls->zoom_step_norm;
    }
  }
  else if (
    find(controls->slow_keys.begin(), controls->slow_keys.end(), key)
      != controls->slow_keys.end()
  )
  {
    if (action == GLFW_PRESS)
      ++controls->slow_pressed_counter;
    else if (action == GLFW_RELEASE)
      --controls->slow_pressed_counter;

    // Just in case, if some events are not delivered or something
    if (controls->slow_pressed_counter > controls->slow_keys.size())
      controls->slow_pressed_counter = 0;

    controls->slow_pressed = controls->slow_pressed_counter > 0;

    if (controls->fast_pressed && !controls->slow_pressed) {
      controls->move_step = controls->move_step_fast;
      controls->zoom_step = controls->zoom_step_fast;
    } else if (!controls->fast_pressed && controls->slow_pressed) {
      controls->move_step = controls->move_step_slow;
      controls->zoom_step = controls->zoom_step_slow;
    } else {
      controls->move_step = controls->move_step_norm;
      controls->zoom_step = controls->zoom_step_norm;
    }
  }
}


void on_mouse_event_controls_cb(
  MouseControls *controls,

  std::variant<
    MousePosEvent,
    MouseScrollEvent,
    MouseButtonEvent
  > mouse_event
)
{
  visit(PatternMatch {
    [](MousePosEvent ev) -> void {
      cerr << "TODO mouse positioning " << ev.xpos << "x" << ev.ypos << endl;
    },
    [](MouseScrollEvent ev) -> void {
      cerr << "TODO mouse scrolling " << ev.xoffset << "x" << ev.yoffset << endl;
    },
    [](MouseButtonEvent ev) -> void {
      cerr << "TODO mouse button " << ev.button << " " << ev.action << " " << ev.mods << endl;
      //GLFW_MOUSE_BUTTON_LEFT
      //GLFW_PRESS
    }
  }, mouse_event);
}
