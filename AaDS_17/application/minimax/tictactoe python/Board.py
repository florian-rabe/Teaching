class IllegalMove(Exception):
    """To be raised when an invalid move is to be made."""
    pass

class IllegalPlayer(Exception):
    """To be raised when player is not 1 or 2."""
    pass

class Board:
    def __init__(self):
        """Creates a new empty board."""
        self.fields = [[0 for x in range(3)] for y in range(3)]
        self.current = 1

    def __str__(self):
        """Returns a visual representation of the board."""
        symbol = {0: " ", 1: "X", 2: "O"}
        res = "\n"
        for i in range(3):
            for j in range(3):
                # print each cell, with a separator between columns
                res += " " + symbol[self.fields[i][j]] + " "
                res += "|" if j != 2 else ""
            # print separator between rows
            res += "\n" + "-" * (4 * 3 - 1) + "\n" if i != 2 else "\n"
        return res

    def getField(self, row, col):
        """Returns the value of a specific field of the board."""
        return self.fields[row][col]

    def getCurrentPlayer(self):
        """Returns the current player of the game."""
        return self.current

    def simMove(self, row, col, player):
        """Simulates a move, only to be called by the Smart Player logic."""
        self.fields[row][col] = player

    def makeMove(self, row, col):
        """Makes a move for the current player. May raise IllegalMove."""
        # Check for illegal move
        if row < 0 or col < 0 or row > 2 or col > 2 or self.fields[row][col] != 0:
            raise IllegalMove
        else:
            # make move
            self.fields[row][col] = self.current
            # change current player
            opponent = {1:2,2:1}
            self.current = opponent[self.current]

    def hasWon(self, player):
        """Returns boolean whether player has won. May raise IllegalPlayer."""
        if player != 1 and player != 2:
            raise IllegalPlayer
        else:
            # diagonal
            diagonal = [i for i in range(3) if (self.fields[i][i] == player)]
            if len(diagonal) == 3:
                return True
            # check other diagonal
            otherDiag = [i for i in range(3) if self.fields[i][3-1-i] == player]
            if len(otherDiag) == 3:
                return True
            # check rows
            for row in range(3):
                x = [col for col in range(3) if self.fields[row][col] == player]
                if len(x) == 3:
                    return True
            # check columns
            for col in range(3):
                x = [row for row in range(3) if self.fields[row][col] == player]
                if len(x) == 3:
                    return True
            # checked everything
            return False

    def isFull(self):
        """Returns boolean whether board is full, i.e. every field is set."""
        return not any(0 in b for b in self.fields)

    def getResult(self):
        """Returns 1, 2 for player that won. 0 for draw. -1 for not done."""
        if self.hasWon(1):
            return 1
        elif self.hasWon(2):
            return 2
        elif self.isFull():
            return 0
        else:
            return -1

    def isOver(self):
        """Returns boolean whether game is over."""
        return self.getResult() >= 0

    def getPossibleMoves(self):
        """Returns list of all possible moves (indices of all empty fields)."""
        return [(r,c) for r in range(3) for c in range(3) if self.getField(r,c) == 0]
