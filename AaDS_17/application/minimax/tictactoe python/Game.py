from Player import *

class Game:
    def __init__(self, player, opponent):
        """Creates a new Game Object with a fresh Board."""
        self.players = [player, opponent]
        self.board = Board()

    def printExampleBoard(self):
        """This prints the instructions with a sample board."""
        print("\nFor your move, please enter a number between 1 and 9.")
        s = ""
        for i in range(3):
            for j in range(3):
                s += " " + str(i*3+j+1) + " "
                if j != 2:
                    s += "|"
            if i != 2:
                s += "\n-----------\n"
        print("\n" + s + "\n")

    def run(self, printing=False):
        """Run Game. printing determines output on the console."""
        log = ""
        resultDict = {0: "  It's a draw!  ", 1: " Player 1 wins! ", 2: " Player 2 wins! "}
        opponent = {1:2,2:1}
        # print instructions
        if printing:
            self.printExampleBoard()
        # start game loop
        while not(self.board.isOver()):
            log += self.board.__str__()
            current = self.board.getCurrentPlayer()
            (r,c) = self.players[current-1].getMove(self.board)
            # make move
            try:
                self.board.makeMove(r,c)
            except IllegalMove:
                # print illegal move handling, if printing is enabled
                if printing:
                    print("Move: ", r, c)
                    print("\n**********************")
                    print("**   Illegal Move!  **")
                    print("** " + resultDict[opponent[current]] + " **")
                    print("**********************\n")
                break
            # print board after each move, if printing is enabled
            print(self.board if printing else "", end="")
        log += self.board.__str__()
        if self.board.getResult() != -1:
            # print the game result, if printing is enabled
            if printing:
                print("**********************")
                print("** " + resultDict[self.board.getResult()] + " **")
                print("**********************\n")
        return (self.board.getResult(), log)
