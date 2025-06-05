from Game import *

def main():
    """Main function in Game, starts the game in the console."""
    logFile = open("log.txt", "w")
    s = SmarterPlayer("smart")
    r = RandomPlayer("random")
    times = 1000
    won1 = 0
    won2 = 0
    dra = 0
    for i in range(times):
        g = Game(r,s)
        (res, log) = g.run()
        if res == 1:
            won1 += 1
            print(log)
            logFile.write(log)
        elif res == 2:
            won2 += 1
        else:
            dra += 1
    print(won1/times, won2/times, dra/times)

# run the main function only if this is __main__
if __name__ == "__main__":
    main()
