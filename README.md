# LearnOpenGL.hs

[LearnOpenGL.com](https://learnopengl.com/) chapters ported to Haskell.

<img src="https://user-images.githubusercontent.com/875324/109432855-d515c980-79d2-11eb-85ce-7b29db58ac72.png" />

## Table of Contents
 - [Demo](#demo)
 - [Motivation](#motivation)
 - [Getting Started](#getting-started)
   - [Hello Window](#hello-window)
   - [Hello Triangle](#HelloTriangle)
   - [Shaders](#Shaders)
   - [Textures](#Textures)
   - [Transformations](#Transformations)
   - [Coordinate Systems](#CoordinateSystems)
   - [Camera](#Camera)
 - [Lighting](#lighting)
   - [Colors](#colors)
   - [Basic Lighting](#basic-lighting)
   - [Materials](#materials)
   - [Lighting Maps](#lighting-maps)
   - [Light Casters](#light-casters)
   - [Multiple Lights](#multiple-lights)
 - [Model Loading](#model-loading)
   - [Assimp](#assimp)
   - [Mesh](#mesh)
   - [Model](#model)
 - [Advanced OpenGL](#advanced-opengl)
 - [Advanced Lighting](#advanced-lighting)
 - [Physically Based Rendering](#physically-based-rendering)
 - [Release](#release)
 - [Develop](#develop)
 - [Performance](#performance)
 - [Contributing](#contributing)
 - [Help](#help)
 - [Vulkan](#vulkan)

## Demo

To see a demo of all examples run `nix-build -A demo && ./result/bin/demo`

### Motivation
[LearnOpenGL.com](https://learnopengl.com) is touted as one of the best learning resources for OpenGL programming. This repository is a first foray into porting the learning resources of https://learnopengl.com to Haskell. The ported examples here do not focus on fancy Haskell abstractions, but attempt to emphasize the domain using the [raw OpenGL C bindings](https://github.com/ekmett/gl) (built by [@ekmett](https://github.com/ekmett)). The code is intended to be simple (and sometimes ugly) to demonstrate what's necesary to effectively use C and Haskell together in OpenGL. To help contribute to this effort please see [Help](#help).

### Getting Started
See [Hello Window](#hello-window).

#### Hello Window
  - Link
    - https://learnopengl.com/Getting-started/Hello-Window
  - Haskell Implementation
    - [Hello Window](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/HelloWindow)

#### Hello Triangle
  - Link
    - https://learnopengl.com/Getting-started/Hello-Triangle
  - Haskell Implementation
    - [Hello Triangle](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Hello-Triangle)
    - [Hello Triangle Ex1](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Hello-Triangle-Ex1)
    - [Hello Triangle Ex2](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Hello-Triangle-Ex2)
    - [Hello Triangle Ex3](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Hello-Triangle-Ex3)
    - [Hello Triangle Idx](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Hello-Triangle-Idx)

#### Shaders
  - Link
    - https://learnopengl.com/Getting-started/Shaders
  - Haskell Implementation
    - [Shaders-Ex1](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/ShadersEx1)
    - [Shaders-Ex2](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/ShadersEx2)
    - [Shaders-Ex3](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/ShadersEx3)

#### Textures
  - Link
    - https://learnopengl.com/Getting-started/Textures
  - Haskell Implementation
    - [Textures](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Textures)
    - [Textures-Ex1](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Textures-Ex1)
    - [Textures-Ex2](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Textures-Ex2)
    - [Textures-Ex3](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Textures-Ex3)
    - [Textures-Ex4](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Textures-Ex4)
    - [Textures-Happy](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Textures-Happy)

#### Camera
  - Link
    - https://learnopengl.com/Getting-started/Camera
  - Haskell Implementation
    - [Camera](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Camera)
    - [Camera-Ex1](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Camera-Ex1)
    - [Camera-Ex2](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Camera-Ex2)

#### Transformations
  - Link
    - https://learnopengl.com/Getting-started/Transformations
  - Haskell implementation
    - [Transformations](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Transformations)
    - [Transformations-Spin](https://github.com/dmjio/LearnOpenGL.hs/tree/master/GettingStarted/Transformations-Spin)

### Lighting
  - Link
    - https://learnopengl.com/Lighting
  - See [Colors](#colors)

#### Colors
  - Link
    - https://learnopengl.com/Lighting/Colors
  - Haskell implementation
    - [Colors](https://github.com/dmjio/LearnOpenGL.hs/tree/master/Lighting/Colors)

#### Basic Lighting
Not yet implemented . Please [help](#help).

#### Materials
Not yet implemented . Please [help](#help).

#### Lighting Maps
Not yet implemented . Please [help](#help).

#### Light Casters
Not yet implemented . Please [help](#help).

#### Multiple Lights
Not yet implemented . Please [help](#help).

### Model Loading
Not yet implemented. Please [help](#help).

#### Assimp
Not yet implemented. Please [help](#help).

#### Mesh
Not yet implemented. Please [help](#help).

#### Model
Not yet implemented. Please [help](#help).

### Advanced OpenGL
Not yet implemented. Please [help](#help).

### Advanced Lighting
Not yet implemented. Please [help](#help).

### Physically Based Rendering
Not yet implemented. Please [help](#help).

## Release

To build the current project and demo script run the following:

```bash
nix-build
```

## Develop

Entering a nix-shell

```bash
nix-shell
```

## Help

Adding new chapters is greatly appreciated, and will go a long way to making Haskell a first class citizen for Game development (with OpenGL).

See [Contributing](#contributing) for more information.

## Contributing

There are many more advanced chapters to cover, the GettingStarted section being an necessary prerequisite. If one is so inclined to contribute to this effort I recommend doing the following:

  - Clone this repo at https://github.com/dmjio/LearnOpenGL.hs
  - Edit `Main.hs` to be the implementation of a chapter that interests you (the current convention is one module per chapter -- built as an executable).
    - Note: Most chapters build off each other, so going through all examples and building the next one in order is advised.
    - Edit the shaders found in the top-level current directory of the project (or add your own as you see fit).
  - Build the project with `nix-build -A app` (makes linking the C libraries into your Haskell executable much simpler not so problematic).
      - For more interactive develpment you can call `nix-shell` and then use `cabal run opengl-exp` to run the project (linking shouldn't be a problem w/ `nix`).
      - Note: the shaders are loaded dynamically.
	  - Calling `nix-build -A app` will only build the top level `opengl-exp` project.
  - When done with your implementation, copy all assets from the top-level `opengl-exp` (Main.hs, shaders, etc.) into its own subfolder.
    - For example, `Lighting/YourChapterGoesHere/{Main.hs,vert.glsl,etc..}`.
	- To see an example of script that does this for you check [create.sh](https://github.com/dmjio/LearnOpenGL.hs/blob/master/create.sh)
  - Update the `default.nix` script to include your chapter in the demo executable.
  - Update the `README.md` with links to your chapter.

## Vulkan

Many people have asked why I focussed on OpenGL instead of Vulkan. The short answer is that despite Vulkan being the future of game development, it is much more difficult (especially for beginners to GPU programming). Additionally, most Vulkan learning resources have recommended OpenGL as a good learning prerequisite. OpenGL also has a much larger body of knowledge currently, making it a good first step in the learning journey. Lastly, https://learnvulkan.com/ would be a good Haskell port to have as well, when it reaches maturity.
