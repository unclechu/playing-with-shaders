PREFIX := /usr/local
TARGET := app
BUILD_DIR := build
SHADERS_EMBED_DIR := shaders-embed

CPP_STANDARD := c++17
CXX := g++
LIBS = $(shell pkg-config --cflags --libs glfw3 opengl glew)
CXX_FLAGS := -O2 -Wall -Wextra -std='$(CPP_STANDARD)' -Isrc -I'$(SHADERS_EMBED_DIR)' $(LIBS)

SRC := $(wildcard src/*.cpp src/*/*.cpp src/*/*/*.cpp)
OBJ := $(patsubst %.cpp,%.o,$(patsubst src/%,$(BUILD_DIR)/%,$(SRC)))

SRC_SHADERS := $(wildcard src/shaders/*.glsl src/shaders/*/*.glsl src/shaders/*/*/*.glsl)
CPP_SHADERS := $(patsubst %.glsl,%.cpp,$(patsubst src/shaders/%,$(SHADERS_EMBED_DIR)/shaders/%,$(SRC_SHADERS)))

all: clean build

clean: clean-build clean-shaders

clean-build:
	rm -rfv -- '$(BUILD_DIR)'

clean-shaders:
	rm -rfv -- '$(SHADERS_EMBED_DIR)'

$(BUILD_DIR)/%.o: src/%.cpp
	mkdir -pv -- '$(dir $@)'
	'$(CXX)' $(CXX_FLAGS) -o '$@' -c '$<'

$(SHADERS_EMBED_DIR)/shaders/%.cpp: src/shaders/%.glsl
	mkdir -pv -- '$(dir $@)'
	xxd -i -- '$<' > '$@'

build: $(CPP_SHADERS) $(OBJ)
	mkdir -pv -- '$(BUILD_DIR)'
	'$(CXX)' $(CXX_FLAGS) -o '$(BUILD_DIR)/$(TARGET)' $(OBJ)

run: build
	'$(BUILD_DIR)/$(TARGET)'

run-mandelbrot: build
	'$(BUILD_DIR)/$(TARGET)' mandelbrot

install: build
	cp -- '$(BUILD_DIR)/$(TARGET)' '$(PREFIX)/bin/$(TARGET)'

info:
	@echo "SRC: ${SRC}"
	@echo "OBJ: ${OBJ}"
