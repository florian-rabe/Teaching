import Foundation

class Board {
    
    enum Result {
        case playerWon
        case opponentWon
        case draw
        case undecided
    }
    
    var fields: [[Int]]
    var current: Int
    
    init() {
        self.fields = Array(repeating: Array(repeating: 0, count: 3), count: 3)
        self.current = 1
    }
    
    init(fields: [[Int]], current: Int) {
        self.fields = fields
        self.current = current
    }
    
    func printBoard() {
        print(fields[0][0], fields[0][1], fields[0][2], separator: " | ", terminator: "\n")
        print(fields[1][0], fields[1][1], fields[1][2], separator: " | ", terminator: "\n")
        print(fields[2][0], fields[2][1], fields[2][2], separator: " | ", terminator: "\n\n")
    }
    
    func getField(row: Int, col: Int) -> Int {
        return self.fields[row][col]
    }
    
    func getCurrentPlayer() -> Int {
        return self.current
    }
    
    func simulateMove(row: Int, col: Int, player: Int) {
        self.fields[row][col] = player
    }
    
    private func getOpponent() -> Int {
        if self.current == 1 {
            return 2
        } else {
            return 1
        }
    }
    
    func makeMove(row: Int, col: Int) {
        self.fields[row][col] = self.current
        self.current = self.getOpponent()
    }
    
    func hasWon(player: Int) -> Bool {
        // diagonal
        var count = 0
        for i in 0...2 {
            if self.fields[i][i] == player {
                count += 1
            }
        }
        if count == 3 {
            return true
        }
        // other diagonal
        count = 0
        for i in 0...2 {
            if self.fields[i][2-i] == player {
                count += 1
            }
        }
        if count == 3 {
            return true
        }
        // check rows
        for row in 0...2 {
            count = 0
            for col in 0...2 {
                if self.fields[row][col] == player {
                    count += 1
                }
            }
            if count == 3 {
                return true
            }
        }
        // check columns
        for col in 0...2 {
            count = 0
            for row in 0...2 {
                if self.fields[row][col] == player {
                    count += 1
                }
            }
            if count == 3 {
                return true
            }
        }
        // everything checked
        return false
    }
    
    func isFull() -> Bool {
        for row in 0...2 {
            for col in 0...2 {
                if self.fields[row][col] == 0 {
                    return false
                }
            }
        }
        return true
    }
    
    func getResult() -> Result {
        if self.hasWon(player: 1) {
            return .playerWon
        } else if self.hasWon(player: 2) {
            return .opponentWon
        } else if self.isFull() {
            return .draw
        } else {
            return .undecided
        }
    }
    
    func isOver() -> Bool {
        return (self.getResult() != .undecided)
    }
    
    func getPossibleMoves() -> [(Int, Int)] {
        var possibleMoves = [(Int, Int)]()
        for row in 0...2 {
            for col in 0...2 {
                if self.fields[row][col] == 0 {
                    possibleMoves.append((row, col))
                }
            }
        }
        return possibleMoves
    }
}

class Player {
    var name: String
    
    init(name: String) {
        self.name = name
    }
    
    init() {
        self.name = ""
    }
    
    func getName() -> String {
        return self.name
    }
    
    func getMove(board: Board) -> (Int, Int) {
        // This method must be overriden!
        return (0,0)
    }
}

class RandomPlayer: Player {
    override func getMove(board: Board) -> (Int, Int) {
        let possibleMoves = board.getPossibleMoves()
        let randomIndex = Int(arc4random_uniform(UInt32(possibleMoves.count)))
        return possibleMoves[randomIndex]
    }
}

class SmartPlayer: Player {
    override func getMove(board: Board) -> (Int, Int) {
        let turn = board.getCurrentPlayer()
        var noLossMoves = [(Int, Int)]()
        let possibleMoves = board.getPossibleMoves()
        if possibleMoves.count == 9 {
            return (1,1)
        } else {
            // check winning move
            for (row, col) in possibleMoves {
                board.simulateMove(row: row, col: col, player: turn)
                if board.hasWon(player: turn) {
                    board.simulateMove(row: row, col: col, player: 0)
                    return (row, col)
                }
                board.simulateMove(row: row, col: col, player: 0)
            }
            // check prevent loss move
            for (row, col) in possibleMoves {
                var opponent = 1
                if turn == 1 {
                    opponent = 2
                }
                board.simulateMove(row: row, col: col, player: opponent)
                if board.hasWon(player: opponent) {
                    board.simulateMove(row: row, col: col, player: 0)
                    noLossMoves.append((row, col))
                }
                board.simulateMove(row: row, col: col, player: 0)
            }
            if noLossMoves.count != 0 {
                return noLossMoves[0]
            }
            // return random move
            let randomIndex = Int(arc4random_uniform(UInt32(possibleMoves.count)))
            return possibleMoves[randomIndex]
        }
    }
}

class SmarterPlayer: Player {
    
    var player = -1
    var opponent = -1
    var choice = (-1,-1)
    
    func evaluateGameState(board: Board, depth: Int) -> Int {
        // evaluate and return game state
        if board.hasWon(player: player) {
            return 10 - depth
        } else if board.hasWon(player: opponent) {
            return depth - 10
        } else {
            return 0
        }
    }
    
    func minimax(board: Board, depth: Int) -> Int {
        // minimax algorithm to not lose
        if board.isOver() {
            return evaluateGameState(board: board, depth: depth)
        }
        let dep = depth + 1
        var scores = [Int]()
        var moves = [(Int, Int)]()
        let possibleMoves = board.getPossibleMoves()
        // if divisible by two, that means it's player's turn. If not, it's the opponent's turn.
        let playersTurn = (depth % 2 == 0)
        
        for (row, col) in possibleMoves {
            // simulate move
            board.simulateMove(row: row, col: col, player: (playersTurn) ? player : opponent)
            // then, calculate the score of the board after making this move
            let score = minimax(board: board, depth: dep)
            scores.append(score)
            moves.append((row, col))
            // delete move again
            board.simulateMove(row: row, col: col, player: 0)
        }
        
        // return the minimum or maximum score (depending on whether its the players turn or the opponents)
        let index = scores.index(of: (playersTurn) ? scores.max()! : scores.min()!)!
        choice = moves[index]
        return (playersTurn) ? scores.max()! : scores.min()!
    }
    
    override func getMove(board: Board) -> (Int, Int) {
        //minor enhancement in the early stages of the game
        let possibleMoves = board.getPossibleMoves()
        if possibleMoves.count == 9 {
            return (1,1)
        } else if possibleMoves.count == 8 {
            return (board.getField(row: 1, col: 1) == 0) ? (1,1) : (0,0)
        }
        player = board.getCurrentPlayer()
        opponent = (player == 1) ? 2 : 1
        _ = minimax(board: board, depth: 0)
        return choice
    }
}

// test smart player
var playerOne = 0
var playerTwo = 0
var draw = 0
for _ in 0...100 {
    let a = RandomPlayer()
    let b = SmarterPlayer()
    var board = Board()
    while !board.isOver() {
        if board.getCurrentPlayer() == 1 {
            // get player a's move
            var (moveX, moveY) = a.getMove(board: board)
            board.makeMove(row: moveX, col: moveY)
        } else {
            // get player b's move
            var (moveX, moveY) = b.getMove(board: board)
            board.makeMove(row: moveX, col: moveY)
        }
        if board.isOver() {
            let result = board.getResult()
            if result == .draw {
                draw += 1
            } else if result == .opponentWon {
                playerTwo += 1
            } else if result == .playerWon {
                playerOne += 1
            } else {
                print("ERRORRRR!")
            }
        }
    }
}

print(playerOne, playerTwo, draw)
