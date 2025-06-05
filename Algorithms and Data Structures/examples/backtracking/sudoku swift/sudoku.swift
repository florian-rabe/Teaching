// represented a sudoku board as a list of list of integers

import Foundation

func solution(_ fields: [[Int]]) -> Bool {
    for i in 0...8 {
        for j in 0...8 {
            if fields[i][j] == 0 {
                return false
            }
        }
    }
    return true
}

func correctMove(_ fields: [[Int]], _ row: Int, _ col: Int, _ num: Int) -> Bool {
    // check row.
    for i in 0...8 {
        if fields[row][i] == num {
            return false
        }
    }
    // check col.
    for i in 0...8 {
        if fields[i][col] == num {
            return false
        }
    }
    // check square.
    let topLeftX = Int(row / 3) * 3
    let topLeftY = Int(col / 3) * 3
    for i in 0...2 {
        for j in 0...2 {
            if fields[topLeftX+i][topLeftY+j] == num {
                return false
            }
        }
    }
    // all good -> correct move
    return true
}

func choices(_ fields: [[Int]], _ row: Int, _ col: Int) -> [Int] {
    var possibleMoves = [Int]()
    for pos in 1...9 {
        if correctMove(fields, row, col, pos) {
            possibleMoves.append(pos)
        }
    }
    return possibleMoves
}

func search(_ fields: [[Int]]) -> [[Int]]? {
    if solution(fields) {
        return fields
    } else {
        var fieldz = fields
        var freeX = -1
        var freeY = -1
        for i in 0...8 {
            for j in 0...8 {
                if fields[i][j] == 0 {
                    freeX = i
                    freeY = j
                }
            }
        }
        for choice in choices(fields, freeX, freeY) {
            fieldz[freeX][freeY] = choice
            let x = search(fieldz)
            if x != nil {
                return x
            }
            fieldz[freeX][freeY] = 0
        }
        return nil
    }
}

func parseBoard(board: String) -> [[Int]] {
    var fields = Array(repeating: Array(repeating: 0, count: 9), count: 9)
    var i = 0
    for char in board.characters {
        if let number = Int(String(char)) {
            fields[i/9][i%9] = number
        }
        i += 1
    }
    return fields
}

let solvedBoard = parseBoard(board: "726493815315728946489651237852147693673985124941362758194836572567214389238579461")
let board = parseBoard(board: "000490815315720940489651207852140090673985124941300758194806572000214389038579460")

if let result = search(board) {
  print(result)
  print(solvedBoard)
}
