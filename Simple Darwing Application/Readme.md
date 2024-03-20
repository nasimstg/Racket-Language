# Drawing Application

This is a simple drawing application built in Racket, a dialect of Lisp. The application allows users to draw shapes on a canvas using a toolbar.

## Features

- Draw different shapes: Circle, Rectangle, Square, Triangle
- Move shapes around the canvas
- Select different tools from the toolbar

## Code Structure

The code is structured around a few key concepts:

- **World**: Represents the state of the application, including all the shapes drawn so far and the currently selected tool.
- **Shape**: Represents a shape that can be drawn. This is a generic structure that can represent different types of shapes (Circle, Rectangle, Square, Triangle).
- **Toolbar**: A part of the application where users can select the tool they want to use.

## How to Run

To run this application, you need to have Racket installed on your machine. Once you have Racket installed, you can run the application by executing the `app.rkt` file.

```bash
racket app.rkt
```

This will start the application and open a window where you can start drawing.

## Contributing

Contributions are welcome. Please feel free to submit a pull request or open an issue.