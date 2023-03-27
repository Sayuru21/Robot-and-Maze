/* Maze
 * Author: Sayuru_W1790000
 * Creation date: 01/02/2023
 */

MACHINE
    Maze
    
CONSTANTS
    x_axis_range_of_maze, y_axis_range_of_maze, maze_area, internal_walls_of_maze, free_square_of_maze, initial_square_of_maze, exit_square_of_maze
    
PROPERTIES
   
    // Space of maze
    x_axis_range_of_maze = 1..7 &
    y_axis_range_of_maze = 1..5 &
    maze_area = x_axis_range_of_maze * y_axis_range_of_maze &
    
    // Walls in the maze
    internal_walls_of_maze <: maze_area &

    internal_walls_of_maze = {(1 |-> 3), (2 |-> 1), (2 |-> 3), (2 |-> 5), (3 |-> 3), (4 |-> 2), (4 |-> 3), (4 |-> 4), (6 |-> 1), (6 |-> 2), (6 |-> 4), (7 |-> 4)} &
 
    // Free space of maze
    free_square_of_maze <: maze_area &
    free_square_of_maze /\ internal_walls_of_maze = {} &
    free_square_of_maze \/ internal_walls_of_maze = maze_area &
    
    // Initial square of maze and exit square of maze
    initial_square_of_maze : free_square_of_maze &
    initial_square_of_maze : maze_area &
    initial_square_of_maze = 1|->1 &
    
    exit_square_of_maze : free_square_of_maze &
    exit_square_of_maze : maze_area &
    exit_square_of_maze = 1|->5
    
END

