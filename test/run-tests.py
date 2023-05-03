#!/usr/bin/env python3
import sys
import json
import subprocess

# Assumes the file is run from the tests directory, and that the executable
# is located at "../mbcheck"

def error(msg):
    print(msg, file=sys.stderr)
    sys.exit(-1)

def run_tests(testsuite):
    overall_result = True
    executable = "../mbcheck"
    # Runs a test group, checking the exit code
    # Later, we may wish to also check stdout and stderr
    def run_group(group):
        print("===", "Group:", group["group"], "===")
        for test in group["tests"]:
            process_result = \
                subprocess.run([executable, test["filename"]], \
                               stdout=subprocess.DEVNULL, \
                               stderr=subprocess.DEVNULL)
            result = True
            if "exit_code" in test:
                result = (result and process_result.returncode == test["exit_code"])
            result_str = "PASS" if result else "FAIL"
            print(f"{test['name']}: ({result_str})")
            if not result:
                overall_result = False

    if "groups" in testsuite:
        for group in testsuite["groups"]:
            run_group(group)
    else:
        error("Malformed testsuite: expected 'groups'")


def main():
    # Default test suite is tests.json
    test_suite = "tests.json"
    # Can optionally be given as a command-line argument
    if len(sys.argv) > 1:
        test_suite = sys.argv[1]

    # Open and parse test suite, then run
    with open(test_suite, 'r') as testsuite:
        parsed = json.loads(testsuite.read())
        run_tests(parsed)

if __name__ == "__main__":
    main()
