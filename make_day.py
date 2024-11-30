import sys
import os
from datetime import date
from urllib.request import Request, urlopen
import fileinput

# usage: python3 ./make_day.py 2022 3

def make_day(year, day):
    days = {"1": "One", "2": "Two", "3": "Three", "4": "Four", "5": "Five",
    "6": "Six", "7": "Seven", "8": "Eight", "9": "Nine", "10": "Ten",
    "11": "Eleven", "12": "Twelve", "13": "Thirteen", "14": "Fourteen", "15": "Fifteen",
    "16": "Sixteen", "17": "Seventeen", "18": "Eighteen", "19": "Nineteen", "20": "Twenty",
    "21": "TwentyOne", "22": "TwentyTwo", "23": "TwentyThree", "24": "TwentyFour", "25": "TwentyFive"
    }
    day_string = days[day]
    file_path = f"./src/main/scala/AOC_{year}/Day{day_string}.scala"
    input_path = f"./src/test/resources/{year}/Day{day}Input.txt"
    if (os.path.exists(file_path)):
        print(f'File {file_path} already exists')
    else:
       with open(f"./utils/day_challenge_template.txt") as template:
           lines = template.read()
           to_write = lines.replace("[[YEAR]]", year).replace("[[DAY]]", day_string)
           print(f"Created new file {file_path} with template")
       with open(file_path, 'x') as file:
           file.write(to_write)
    if (os.path.exists(input_path)):
        print(f"Input file {input_path} already exists")
    else:
        f = open(input_path, 'x')
        f.close
        print(f"Created new empty file {input_path} with template")
    with open(input_path, "a+") as input_file:
        requested_date = date(int(year), 12, int(day))
        existing_content = input_file.read()
        input_file.seek(0)
        if input_file.read() == "" and date.today() >= requested_date:
            print("10/10 would fetch")
            req = Request(f"https://adventofcode.com/{year}/day/{day}/input")
            with open("./session_cookie.txt", "r") as cookie_file:
                cookie = cookie_file.read()
            req.add_header('Cookie', f'session={cookie}')
            content = urlopen(req).read().decode('utf-8')
            print("got input data")
            input_file.write(content)
        else:
            print(f"either input file {input_path} is already populated or date is in the future")


    print("uncommenting line in controller")
    file_path = f"./src/main/scala/AOC_{year}/Controller{year}.scala"
    substring = f"case \"{day}\""

    with fileinput.FileInput(file_path, inplace=True) as file:
        for line in file:
            if substring in line and "//" in line:
                print(line.replace("//", "", 1), end='')  # Remove the first occurrence of "//"
            else:
                print(line, end='')


if __name__ == "__main__":
    make_day(sys.argv[1], sys.argv[2])
