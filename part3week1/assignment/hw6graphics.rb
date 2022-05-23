# University of Washington, Programming Languages, Homework 6, hw6graphics.rb

# This file provides an interface to a wrapped Tk library. The auto-grader will
# swap it out to use a different, non-Tk backend.

require 'tk'

# class TetrisRoot
class TetrisRoot
    # constructor
    def initialize
      # create a root framework of 
      # height: 615px, 
      # width: 205px, 
      # background: 'lightblue', 
      # title: "Tetris"
      @root = TkRoot.new('height' => 615, 'width' => 205, 
               'background' => 'lightblue') {title "Tetris"}    
    end
  
    # ?bind the root with the char and cb
    def bind(char, callback)
      @root.bind(char, callback)
    end
  
    # Necessary so we can unwrap before passing to Tk in some instances.
    # Student code MUST NOT CALL THIS.
    attr_reader :root
  end
  
  # subclass: Tetris Timer
  class TetrisTimer
    # initialize a timer
    def initialize
      @timer = TkTimer.new
    end
  
    # stop the timer
    def stop
      @timer.stop
    end
  
    # start the timer in (delay)? seconds and 
    # call the cb
    def start(delay, callback)
      @timer.start(delay, callback)
    end
  end
  
  # subclass Tetrics Canvas
  class TetrisCanvas
    # initialize the canvas with background: grey
    def initialize
      @canvas = TkCanvas.new('background' => 'grey')
    end
    
    # place the canvas in the on the root framework
    def place(height, width, x, y)
      @canvas.place('height' => height, 'width' => width, 'x' => x, 'y' => y)
    end
  
    # unplace the canvas
    # what does unplace mean?
    def unplace
      @canvas.unplace
    end
  
    # delete the canvas itself?
    def delete
      @canvas.delete
    end
  
    # Necessary so we can unwrap before passing to Tk in some instances.
    # Student code MUST NOT CALL THIS.
    attr_reader :canvas
  end
  
  # subclass Tetris Label
  class TetrisLabel
    # initialize Tetris Lable with wrapped roote and options
    # Q: what does the `&` mean here?
    def initialize(wrapped_root, &options)
      unwrapped_root = wrapped_root.root
      @label = TkLabel.new(unwrapped_root, &options)
    end
  
    # place the label in the position specified
    def place(height, width, x, y)
      @label.place('height' => height, 'width' => width, 'x' => x, 'y' => y)
    end
  
    # specify the text content for the Tetris Label
    def text(str)
      @label.text(str)
    end
  end
  
  # subclass Tetris Button
  class TetrisButton
    # initialize button
    def initialize(label, color)
      @button = TkButton.new do 
        text label
        background color
        command (proc {yield}) # ?
      end
    end
    
    # place button
    def place(height, width, x, y)
      @button.place('height' => height, 'width' => width, 'x' => x, 'y' => y)
    end
  end
  
  # initialize tetris rectangle
  class TetrisRect
    def initialize(wrapped_canvas, a, b, c, d, color)
      unwrapped_canvas = wrapped_canvas.canvas
      @rect = TkcRectangle.new(unwrapped_canvas, a, b, c, d, 
                               'outline' => 'black', 'fill' => color)
    end
    
    # remove the rectangle
    def remove
      @rect.remove
    end
  
    # move the rectangle
    def move(dx, dy)
      @rect.move(dx, dy)
    end
  
  end
  
  def mainLoop
    Tk.mainloop # ? where is Tk defined
  end
  
  def exitProgram
    Tk.exit
  end