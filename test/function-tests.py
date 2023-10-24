import re
import subprocess
import unittest
import tempfile
import os

def run_mbcheck(program_str):
    with tempfile.NamedTemporaryFile(delete=False, suffix='.pat') as temp_file:
        temp_file.write(program_str.encode())
        file_path = temp_file.name

    try:
        result = subprocess.run(['./mbcheck', file_path], stdout=subprocess.PIPE, text=True)
        output = result.stdout
        match = re.search(r'Final Result: (.+)', output)
        if match:
            return match.group(1).strip()
        else:
            raise ValueError("Final Result not found in output")
    finally:
        os.remove(file_path)

class TestMbcheck(unittest.TestCase):
    
    def test_programs(self):
        test_cases = [
            ("1+2", "3"),
            ("(1+1)==2", "true"),
            ("((1+1)==2)&&((1+13)==2)", "false"),
            ("((1+1)==3)||((1+1)==2)", "true"),
            ("let x = 1 in x", "1"),
            ("let x = 5 in x == x", "true"),
            ("let x = 6 in x != x", "false"),
            ("let x = true in x && x", "true"),
            ("let x = false in x || x", "false"),
            ("let x = 7 in let y = 8 in x + y", "15"),
            ("let x = 9 in let y = 10 in x - y", "-1"),
            ("let x = 11 in let y = 12 in x * y", "132"),
            ("let x = 13 in let y = 14 in x / y", "0"),
            ("let x = 15 in let y = 15 in x == y", "true"),
            ("let x = 16 in let y = 17 in x != y", "true"),
            ("(fun (x: Int): Int { x + 1 })(5)","6"),
            ("(fun (x: Int, y: Int): Int { x + y })(5,6)","11"),
            ("def main(): Int {let x: Int = 5 in x + x} main()", "10"),
            ("def main(): Int {let x: Int = let y = 5 in y in x + x} main()", "10"),
            ("def main(x: Int, y: Int):Int {x + y} main(10, 15)","25"),
            ("def main(x: Int, y: Int):Int {let z = 9 in x + y + z } main(10, 15)","34"),
            ("def main(): Int {let x  = 18 in if(x <= 19) { x + 20 } else { x - 21}} main()","38"),
            ("def main(): Int {let x  = 22 in if(x >= 23) { x + 24  } else { let y = 22 + x in x + y}} main()","66"),
            ("def main(): Int {let x = (fun (z: Int, y: Int): Int { z + y })(5,6) in x + 10} main()", "21"),
            ("def main(): Int {let x = (fun (z: Int, y: Int): Int { z + y })(5,6) in if(x == 19) { x + 20 } else { x - 21}} main()", "-10"),
            
            ]
    
        for program_str, expected_output in test_cases:
            with self.subTest(program_str=program_str):
                try:
                    actual_output = run_mbcheck(program_str)
                    self.assertEqual(actual_output, expected_output)
                except AssertionError as e:
                    print(f"Test failed for program: {program_str}")
                    print(f"Expected: {expected_output}")
                    print(f"Actual: {actual_output}")
                    raise  


if __name__ == "__main__":
    unittest.main()
