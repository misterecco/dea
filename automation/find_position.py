import argparse

def find_position(file, position):
    line = 1
    col = -1

    for l in open(file):
        if (len(l) >= position):
            col = position + 1
            break
        else:
            position -= len(l)
            line += 1

    if col == -1:
        return (-1, -1)
    return line, col


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('file', help="File to open")
    parser.add_argument('position', help="Position in file (character number)")

    args = parser.parse_args()

    (line, col) = find_position(args.file, int(args.position))

    print(f"{args.file}:{args.position} -> {line},{col}")
