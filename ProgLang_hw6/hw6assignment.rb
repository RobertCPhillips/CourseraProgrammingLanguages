# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  Singular_Block = [[[0,0]]]
  
  All_My_Pieces = All_Pieces + [Singular_Block, #single block
                                [[[0,0],[-1,0],[-2,0],[1,0],[2,0]],  #5 long block horiz
                                 [[0,0],[0,-1],[0,-2],[0,1],[0,2]]],  #5 long block vert
                                 rotations([[0, 0], [-1, 0], [1, 0], [0,-1], [-1, -1]]), # square plus 1 block
                                 rotations([[0, 0], [1, 0], [0, -1]])] # 3 block
  
  # your enhancements here
  def initialize (point_array, board)
    super(point_array, board)
  end
  
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
  
    def self.next_piece_as_singular (board)
    MyPiece.new(Singular_Block, board)
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @iamcheating = false
  end
  
  def next_piece
    puts "myboard next piece"
    if @iamcheating 
      puts "i am cheating"
      @current_block = MyPiece.next_piece_as_singular(self)
      @iamcheating = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end
  
  def cheat ()
    if @score >= 100
      @score -= 100
      @current_block = MyPiece.next_piece_as_singular(self)
      #@iamcheating = true
    end
  end
  
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    blockSize = locations.size - 1
    puts blockSize + 1
    (0..blockSize).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
  
end

class MyTetris < Tetris
  # your enhancements here
  def initialize
    super
  end
  
  def key_bindings  
    super
    @root.bind('u', proc {@board.rotate_clockwise;@board.rotate_clockwise;}) 
    @root.bind('c', proc {@board.cheat})
  end
  
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  
end


