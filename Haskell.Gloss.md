# Haskell.Gloss

> 假设你已经在windows中安装了haskell，以及cabal等工具包。如未安装，可参考: [安装教程](https://www.haskell.org/ghcup/)

## 引言

`Gloss` 是一个专为 Haskell 语言设计的轻量级图形库，旨在简化 2D 图形应用程序的开发。它提供了直观且易于使用的接口，使开发者能够快速绘制图形、处理用户输入和管理窗口，而无需深入了解底层的图形处理细节。Gloss 支持跨平台运行，适用于 Windows、macOS 和 Linux 系统，非常适合用于教学、快速原型设计以及开发简单的游戏或图形演示。通过 Gloss，Haskell 开发者可以专注于创意表达，而不必担心复杂的图形编程技术细节。

`Cabal`: 这是 Haskell 生态系统中的一个重要工具，用于管理 Haskell 项目的构建、依赖和打包。它类似于其他编程语言中的包管理工具，如 Python 的 pip。Cabal 使得 Haskell 开发者能够轻松地管理项目的依赖关系、编译代码以及发布库。



## 搭建环境

本段参考来源: [cabal init](https://cabal.readthedocs.io/en/3.4/getting-started.html)

### 1. 创建新项目

首先在电脑中任意位置创建项目文件夹，并取名如“myGlossProject”。

创建完毕之后,进入文件夹，点击上方地址栏并清空地址栏并输入`cmd`，这样就能省去手动输入地址的时间。

**初始化项目**：使用 `cabal init` 命令初始化一个新的 Haskell 项目。这将创建一个包含基本配置文件的项目目录

```cmd
cabal update
cabal init
```

在交互式提示中，你可以选择项目类型、名称、依赖等信息。

**项目结构**：初始化后，Cabal 会生成一个 `.cabal` 文件，这是项目的配置文件，定义了项目的名称、版本、依赖等信息。



### 2. 添加依赖

**编辑 `.cabal` 文件**：在 `.cabal` 文件中，你可以添加项目所需的依赖。例如，要添加 `haskell-say` 以及一会需要用的`gloss`库，可以在 `build-depends` 部分添加

```text
build-depends:       base ^>=4.17.2.1,
					 haskell-say ^>=1.0.0.0,
                     gloss >= 1.13.2.2
```

**安装依赖**：运行以下命令来安装项目所需的所有依赖：

```cmd
cabal install haskell-say
cabal install gloss
```



### 3. 构建和运行项目

**构建项目**：使用 `cabal build` 命令编译项目：

```cmd
cabal build
```

**运行项目**：构建成功后，可以使用 `cabal run` 命令运行项目：

```cmd
cabal run
```

如果成功输出Hello, Haskell. 说明项目创建成功。



### 4. 安装FreeGlut

FreeGLUT 是一个开源的图形库, 安装FreeGlut是在使用 OpenGL 进行图形编程时的一个重要步骤，尤其是在使用 Haskell 的图形库（如 Gloss）时，FreeGLUT 提供了必要的窗口管理和输入处理功能。

在使用 MSYS2 环境下安装 FreeGLUT 是一个相对简单的过程，因为 MSYS2 提供了一个类似于 Linux 的包管理系统，可以方便地安装各种库。

**下载并安装 MSYS2**：

- 从 [MSYS2 的官方网站](https://www.msys2.org/) 下载适用于 Windows 的安装程序。
- 按照安装向导完成安装。

**更新 MSYS2**：

- 打开 MSYS2 MSYS 或 MSYS2 MinGW 64-bit Shell。

- 运行以下命令来更新软件包数据库和基础系统：

  ```bash
  pacman -Syu
  ```

- 如果提示关闭终端，请关闭并重新打开，然后再次运行更新命令以确保所有内容都是最新的。

**安装 FreeGLUT**：

- 在 MSYS2 MinGW 64-bit Shell 中，运行以下命令以安装 FreeGLUT：

  ```bash
  pacman -S mingw-w64-x86_64-freeglut
  ```

**添加系统环境变量** - windows

- **找到 FreeGLUT 的路径**：

  - 如果你通过 MSYS2 安装了 FreeGLUT，通常它的库文件位于 MSYS2 的安装目录下的 `mingw64/bin` 或 `mingw32/bin` 中，具体取决于你安装的是 64 位还是 32 位版本。

- **复制路径**：

  - 打开文件资源管理器，导航到 MSYS2 的安装目录，例如 `C:\msys64\mingw64\bin`。
  - 复制该路径。

- **打开系统环境变量设置**：

  - 右键点击“此电脑”或“我的电脑”，选择“属性”。
  - 点击“高级系统设置”。
  - 在“系统属性”窗口中，点击“环境变量”。

- **编辑 PATH 变量**：

  - 在“系统变量”部分，找到名为 `Path` 的变量，选中并点击“编辑”。
  - 在“编辑环境变量”窗口中，点击“新建”并粘贴你之前复制的 FreeGLUT 路径。
  - 确保路径正确无误，然后点击“确定”保存更改。

- **验证设置**

  - 你可以手动检查 PATH 环境变量中是否包含 FreeGLUT 的路径：

    ```cmd
    echo %PATH%
    ```

  - 查看输出中是否包含了你之前添加的 FreeGLUT 的路径（例如 `C:\msys64\mingw64\bin`）。

### 5. 编写测试程序

接下来，编写一个简单的 Haskell 程序来绘制一个窗口并显示一个基本的图形，例如两个圆。

在IDE中打开项目文件夹，找到Main.hs，并输入以下代码：

```haskell
import Graphics.Gloss

main :: IO ()
main = display window background drawing
  where
    window = InWindow "Gloss Example" (800, 600) (100, 100)
    background = white
    drawing = Pictures [circleSolid 80, translate 150 150 $ color red $ circleSolid 50]
    
```

**运行项目**： 在项目目录中，使用以下命令来构建并运行项目：

```bash
cabal run
```

如果一切设置正确，`cabal run` 将会打开一个窗口，并显示一个黑球，一个红球。



## 基本操作

本段参考来源：[官方文档](https://hackage.haskell.org/package/gloss)

1. 画抽象图（星空，黑洞）
2. 流水线
3. 旋转舞女
4. 3d
