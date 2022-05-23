# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # re-write all pieces
  All_My_Pieces = [
    rotations([[0,0], [1, 0], [0, -1], [-1, 0], [-1, -1]]), 
    [ [[0,0], [1,0], [2, 0], [-1, 0], [-2, 0]], [[0,0], [0,-1], [0, -2], [0, 1], [0, 2]] ],
    rotations([[0,0], [1, 0], [0, -1]])
  ] + All_Pieces

  
  # initialize keep the same
  # current_rotation keep the same
  # moved keep the same
  # position keep the same
  # color keep the same
  # drop by one keep the same
  # move keep the same
  # rotations keep the same
  

  # next_piece enhancement, deal with cheating scenario
  def self.next_piece (board)
    if (board.is_cheating && board.score >= 100) 
      board.is_cheating = false # switch is_cheating flag back to false
      board.update_score(board.score - 100)
      # next piece [[0,0]]
      MyPiece.new([[[0,0]]], board)
    else
      board.is_cheating = false
      MyPiece.new(All_My_Pieces.sample, board)
    end
  end

  ## All_Colors keep the same
end
  
class MyBoard < Board
  # your enhancements here
  def initialize(game)
    super(game)
    @is_cheating = false
    # important! re-initialize the current block.
    # since we have re-wrote next piece logic
    @current_block = MyPiece.next_piece(self)
  end

  def update_score(score)
    @score = score
  end

  # set is_cheating flag
  def cheat
    @is_cheating = true
  end

  # rotates up-side-down
  def rotate_up_side_down
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
  end

  # gets the next piece
  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
  end

  # now we have pieces that have only one point and may also have 5 points
  def store_current 
    locations = @current_block.current_rotation 
    displacement = @current_block.position
    # this re-written dynamically determines how many points are there
    # for current piece
    locations.each_index{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index] 
    }
    remove_filled
    @delay = [@delay - 2, 80].max 
  end

  attr_accessor :is_cheating #expose a getter and setter for cheat variable
end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  
  def key_bindings  
    super
    @root.bind('u', proc {@board.rotate_up_side_down})
    @root.bind('c', proc {@board.cheat})
  end

end