from random import choice
from Board import *

class Player:
    """Abstract Player class."""
    def __init__(self, name):
        """Constructor for a Player Object. Takes only name of Player."""
        self.name = name

    def getName(self):
        """Returns the name of this player."""
        return self.name

    def getMove(self, board):
        """Abstract method."""
        pass

class RandomPlayer(Player):
    def getMove(self, board):
        """Returns a random (possible) move."""
        return choice(board.getPossibleMoves())

class HumanPlayer(Player):
    def getMove(self, board):
        """Returns a move from user-input."""
        m = int(input("Enter move (1-9): "))
        r = (m-1) // 3
        c = (m-1) % 3
        return (r,c)

class SmartPlayer(Player):
    def getMove(self, board):
        """Returns a smart move."""
        turn = board.getCurrentPlayer()
        noLossMoves = []
        opponent = {1:2, 2:1}
        # check if this is the beginning of the game
        if len(board.getPossibleMoves()) == 9:
            return (1,1)
        elif len(board.getPossibleMoves()) == 8:
            if board.getField(1,1) == 0:
                return (1,1)
            else:
                return (0,0)
        # check more complicated game situations
        for (move1, move2) in board.getPossibleMoves():
            # check winning move
            board.simMove(move1, move2, turn)
            if board.hasWon(turn):
                board.simMove(move1, move2, 0)
                return (move1, move2)
            # check prevent loss move
            board.simMove(move1, move2, opponent[turn])
            if board.hasWon(opponent[turn]):
                noLossMoves.append((move1, move2))
            board.simMove(move1, move2, 0)
        # return no loss move // random move
        if len(noLossMoves) > 0:
            return choice(noLossMoves)
        else:
            return choice(board.getPossibleMoves())

class SmarterPlayer(Player):
    def __init__(self, name):
        self.name = name
        self.choice = (-1,-1)
        self.player = -1
        self.opponent = -1

    def score(self, board, depth):
        if board.getResult() == 0:
            return 0
        elif board.getResult() == self.player:
            return 10 - depth
        elif board.getResult() == self.opponent:
            return depth - 10

    def minimax(self, board, depth):
        if board.isOver():
            return self.score(board, depth)
        depth += 1
        scores = []
        moves = []
        playersTurn = (depth % 2 == 0)
        for (row, col) in board.getPossibleMoves():
            if playersTurn:
                board.simMove(row, col, self.player)
            else:
                board.simMove(row, col, self.opponent)
            # calculate score
            scores.append(self.minimax(board, depth))
            moves.append((row, col))
            # delete move again
            board.simMove(row, col, 0)

        if playersTurn:
            self.choice = moves[scores.index(max(scores))]
            return max(scores)
        else:
            self.choice = moves[scores.index(min(scores))]
            return min(scores)

    def getMove(self, board):
        """Returns a smart move."""
        turn = board.getCurrentPlayer()
        noLossMoves = []
        opponent = {1:2, 2:1}
        # check if this is the beginning of the game
        if len(board.getPossibleMoves()) == 9:
            return (1,1)
        elif len(board.getPossibleMoves()) == 8:
            if board.getField(1,1) == 0:
                return (1,1)
            else:
                return (0,0)
        # check more complicated game situations
        for (move1, move2) in board.getPossibleMoves():
            # check winning move
            board.simMove(move1, move2, turn)
            if board.hasWon(turn):
                board.simMove(move1, move2, 0)
                return (move1, move2)
            # check prevent loss move
            board.simMove(move1, move2, opponent[turn])
            if board.hasWon(opponent[turn]):
                noLossMoves.append((move1, move2))
            board.simMove(move1, move2, 0)
        # return no loss move // random move
        if len(noLossMoves) > 0:
            return choice(noLossMoves)
        else:
            # fix! two opposite corners and I'm in the middle
            plMid = board.getField(1,1) == self.player
            opDia = (board.getField(0,0) == self.opponent and board.getField(2,2) == self.opponent)
            opInvDia = (board.getField(0,2) == self.opponent and board.getField(2,0) == self.opponent)
            if (opDia and plMid) or (opInvDia and plMid):
                if len(board.getPossibleMoves()) == 6:
                    return (0,1)
            # instead of making a random move, now we can apply the minimax algorithm
            self.player = board.getCurrentPlayer()
            self.opponent = 2 if self.player == 1 else 1
            self.minimax(board, 0)
            return self.choice
