/**
 * fifteen.c
 *
 * Computer Science 50
 * Problem Set 3
 *
 * Implements Game of Fifteen (generalized to d x d).
 *
 * Usage: fifteen d
 *
 * whereby the board's dimensions are to be d x d,
 * where d must be in [DIM_MIN,DIM_MAX]
 *
 * Note that usleep is obsolete, but it offers more granularity than
 * sleep and is simpler to use than nanosleep; `man usleep` for more.
 */
 
#define _XOPEN_SOURCE 500
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

using namespace std;
// constants
#define dim 8
#define CONS 0

// board
char board[dim][dim];
 int ac_x, ac_y, rp_x, rp_y;

// dimensions
int d;

// prototypes
void clear(void);
void greet(void);
void init(void);
void draw(void);
bool move(int row,int col,int color);
bool won(void);

int main()
{
   


    // greet user with instructions
    greet();

    // initialize the board
    init();
	
	int player =0;
    // accept moves until game is won
    while (true)
    {
        // clear the screen
        clear();

        // draw the current state of the board
        draw();

        // prompt for move
        printf("Position to insert: ");
        int row;
		cin >> row ;
        
        // quit if user inputs 0 (for testing)
        if (row == 0)
        {
			if(won())	
				break;
			else
				printf("Game not over\n");
			continue;
        }
		int col ;
		cin >> col;

        // move if possible, else report illegality
        if (!move(row-1,col-1,player))
        {
            printf("\nIllegal move.\n");
            usleep(500000);
        }
        else
        {
        player = 1 -player;
			//postmove(row,col,player);           
        }
		
        // sleep thread for animation's sake
        usleep(50000);
    }
    


    // success
    return 0;
}

/**
 * Clears screen using ANSI escape sequences.
 */
void clear(void)
{
    printf("\033[2J");
    printf("\033[%d;%dH", 0, 0);
}

/**
 * Greets player.
 */
void greet(void)
{
    clear();
    printf("WELCOME TO Reversi\n");
    usleep(1000000);
}

/**
 * Initializes the game's board with tiles numbered 1 through d*d - 1
 * (i.e., fills 2D array with values but does not actually print them).  
 */
void init(void)
{
   for(int i=0;i<dim;i++)
	for(int j=0;j<8;j++)
		board[i][j]='.';

	board[3][3]=board[4][4] = '0';
	board[3][4]=board[4][3] = '1';
}

/**
 * Prints the board in its current state.
 */
void draw(void)
{
    for(int i=0;i < dim;i++)
    {
        for(int j=0;j < dim;j++)
        {
            printf("%c ",board[i][j]);
        }
        printf("\n");
    }
    
}

void postmove(int row,int col,char color,int val)
{
	board[row][col]=color;
cout << "postmove "<<row<<" " <<col<<" "<< color <<" " << val <<" " << "\n";
    int i=0;
	int pro=0;
	if(val==1)
		for(i=row-1;i>=0;i--)
		{
			if(board[i][col]==color)
			{
				break;
			}	
			else
			{
				board[i][col]=color;
			}
		}
	if(val==2)
		for(i=row+1;i<8;i++)
		{
			if(board[i][col]==color)
			{
				break;
			}
			else
				board[i][col]=color;
		}

	if(val==3)
		for(i=col-1;i>=0;i--)
		{
			if(board[row][i]==color)
			{
				break;
			}
			else
				board[row][i]=color;
		}
	
	if(val==4)
		for(i=col+1;i<8;i++)
		{
			if(board[row][i]==color)
			{
				break;
			}
			else
				board[row][i]=color;
		}	
	
	if(val==5)
		for(i=1;i<min(row,col);i++)
		{
			if(board[row-i][col-i]==color)
			{
				break;
			}
			else 
				board[row-i][col-i]=color;
		}
	
	if(val==6)
		for(i=1;i<7-max(row,col);i++)
		{
			if(board[row+i][col+i]==color)
			{
				break;
			}
			else 
				board[row+i][col+i]=color;
		}

// error HERE FOR SURE 
	if(val==7)
		for(i=1;i<min(row,7-col);i++)
		{
			if(board[row-i][col+i]==color)
			{
				break;
			}
			else
				board[row-i][col+i]=color;
		}
// ERRORRRRRRRRRRRRRRRRRRRRRRRRR	
	if(val==8)
		for(i=1;i<min(7-row,col);i++)
		{
			if(board[row+i][col-i]==color)
			{
				break;
			}
			else
			board[row+i][col-i]=color;
		}
}
/**
 * If tile borders empty space, moves tile and returns true, else
 * returns false. 
 */
 // int ac_x,ac_y,rp_x,rp_y;
bool move(int row,int col,int color)
{
    int i=0;
	int pro=0;
	char value = char(1-color+'0');
	char value1 = (char)color +'0';

	if(row>0&&board[row-1][col]==value)
		for(i=row-1;i>=0;i--)
		{
			if(board[i][col]==value1)
			{
				pro=1;
	    		postmove(row,col,value1,1);
			}
		}
	
	if(row<7&&board[row+1][col]==value)
		for(i=row+1;i<8;i++)
		{
			if(board[i][col]==value1)
			{
				pro=1;
	    		postmove(row,col,value1,2);
			}
		}

	if(col>0&&board[row][col-1]==value)
		for(i=col-1;i>=0;i--)
		{
			if(board[row][i]==value1)
			{
				pro=1;
	    		postmove(row,col,value1,3);
			}
		}
	
	if(col<7&&board[row][col+1]==value)
		for(i=col+1;i<8;i++)
		{
			if(board[row][i]==value1)
			{
				pro=1;
	    		postmove(row,col,value1,4);
			}
		}
	
	
	if(row>0&&col>0&&board[row-1][col-1]==value)
		for(i=1;i<min(row,col);i++)
		{
			if(board[row-i][col-i]==value1)
			{
				pro=1;
	    		postmove(row,col,value1,5);
			}
		}
	
	if(row<7&&col<7&&board[row+1][col+1]==value)
		for(i=1;i<7-max(row,col);i++)
		{
			if(board[row+i][col+i]==value1)
			{
				pro=1;
	    		postmove(row,col,value1,6);
			}
		}

// error HERE FOR SURE 
	if(row>0&&col<7&&board[row-1][col+1]==value)
		for(i=1;i<min(row,7-col);i++)
		{
			if(board[row-i][col+i]==value1)
			{
				pro=1;
	    		postmove(row,col,value1,7);
			}
		}

// ERRORRRRRRRRRRRRRRRRRRRRRRRRR	
	if(row<7&&col>0&&board[row+1][col-1]==value)
		for(i=1;i<min(7-row,col);i++)
		{
			if(board[row+i][col-i]==value1)
			{
				pro=1;
	    		postmove(row,col,value1,8);
			}
		}
	if(pro==1)
		return true;
	return false;

}

/**
 * Returns true if game is won (i.e., board is in winning configuration), 
 * else false.
 */
bool won(void)
{ 
    int ele = 1;
    
    for(int i=0;i < d;i++)
    {
        for(int j=0;j < d;j++)
        {
            if (i == d - 1 && j == d - 1)
            {
                break;
            }    
            if (board[i][j] != ele)
            {
                return false;
            }    
            ele++;
        }
    }
    return true;
}
