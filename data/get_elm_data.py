import requests
import json
import time
import datetime
import re

all_packages = "http://package.elm-lang.org/all-packages"

packages_for_seventeen = "http://package.elm-lang.org/new-packages"

github_json = "https://api.github.com/repos/"

now = datetime.datetime.now()

redirect_matcher = re.compile("(?:elm-lang.org/|github.com/)?(\S+/[^/\.\s]+)")

test_dir = "https://api.github.com/repos/{package}/contents/test?ref=master"
tests_dir = "https://api.github.com/repos/{package}/contents/tests?ref=master"
examples_dir = "https://api.github.com/repos/{package}/contents/examples?ref=master"
elm_package_file = "https://raw.githubusercontent.com/{package}/master/elm-package.json?ref=master"


def get_elm_package_index():
    print "retrieving elm package index"
    with open("primary/package-index.json", "w") as INDEX:
        all_pkgs = requests.get(all_packages)
        if all_pkgs.status_code < 300 and all_pkgs.status_code >= 200:
            INDEX.write(all_pkgs.content)

    with open("primary/new-packages.json", "w") as INDEX:
        all_pkgs = requests.get(packages_for_seventeen)
        if all_pkgs.status_code < 300 and all_pkgs.status_code >= 200:
            INDEX.write(all_pkgs.content)


def wait_for_reset_if_necessary(headers):
    limit = headers.get("X-RateLimit-Limit", None)
    remain = headers.get("X-RateLimit-Remaining", None)
    resetAt = headers.get("X-RateLimit-Reset",None)

    if limit is not None and remain is not None and resetAt is not None:
        if remain == 0:
            now_epoch = time.time()
            waitFor = resetAt - now_epoch
            print "waiting for " + str(waitFor) + " seconds for limits to reset"
            time.sleep(waitFor)


def get_github_data():

    credentials = None
    with open('credentials/github.json') as GITHUB:
        credentials = json.loads(GITHUB.read())
    if credentials is None:
        raise "No github credentials!"

    packages = []
    with open("primary/package-index.json") as INDEX:
        index = INDEX.read()
        packages = json.loads(index)

    print "retrieving github data"
    total = len(packages)
    full_pkg_data = {}
    for i, pkg in enumerate(packages):
        print str(i) + "/" + str(total) + " - Retriving github data for: " + str(pkg["name"])
        pkg_data = requests.get(github_json + pkg["name"], params=credentials)

        github_data = {}
        if pkg_data.status_code < 300 and pkg_data.status_code >= 200:
            github_data = json.loads(pkg_data.content)
        else:
            print "failed retrieval"
        wait_for_reset_if_necessary(pkg_data.headers)

        if github_data:
            # Has Test Directory
            has_test_dir = requests.get(test_dir.format(package=pkg["name"]), params=credentials)
            if has_test_dir.status_code < 300 and has_test_dir.status_code >= 200:
                github_data["has_test_dir"] = len(json.loads(has_test_dir.content)) > 0
            else:
                github_data["has_test_dir"] = False
            wait_for_reset_if_necessary(has_test_dir.headers)

            if github_data["has_test_dir"] is False:
                has_test_dir = requests.get(tests_dir.format(package=pkg["name"]), params=credentials)
                if has_test_dir.status_code < 300 and has_test_dir.status_code >= 200:
                    github_data["has_test_dir"] = len(json.loads(has_test_dir.content)) > 0
                else:
                    github_data["has_test_dir"] = False
                wait_for_reset_if_necessary(has_test_dir.headers)

            # Has Examples Directory
            has_examples_dir = requests.get(examples_dir.format(package=pkg["name"]), params=credentials)
            if has_examples_dir.status_code < 300 and has_examples_dir.status_code >= 200:
                github_data["has_examples_dir"] = len(json.loads(has_examples_dir.content)) > 0
            else:
                github_data["has_examples_dir"] = False
            wait_for_reset_if_necessary(has_examples_dir.headers)

            # Retrieve Elm Package Info
            elm_package = requests.get(elm_package_file.format(package=pkg["name"]), params=credentials)
            if elm_package.status_code < 300 and elm_package.status_code >= 200:
                try:
                    github_data["elm_package"] = json.loads(elm_package.content)
                except ValueError:
                    print "Error parsing elm-package.json for " + pkg["name"]
                    github_data["elm_package"] = {}

            else:
                github_data["elm_package"] = {}
            wait_for_reset_if_necessary(elm_package.headers)

            full_pkg_data[pkg["name"]] = github_data

        time.sleep(0.1)


    with open("primary/github-package-data.json", "w") as INDEX:
        INDEX.write(json.dumps({'retrieved':str(now), 'packages':full_pkg_data}, indent=4))
    

def remove_prefix(text, prefix):
    return text[text.startswith(prefix) and len(prefix):]

def remove_github_prefix(text):
    text = remove_prefix(text, "http://")
    text = remove_prefix(text, "https://")
    text = remove_prefix(text, "github.com/")
    text = remove_prefix(text, "package.elm-lang.org/")
    return text



def extract_metrics():
    packages = []
    with open("primary/package-index.json") as INDEX:
        index = INDEX.read()
        packages = json.loads(index)

    new_packages = []
    with open("primary/new-packages.json") as INDEX:
        index = INDEX.read()
        new_packages = json.loads(index)

    github_data = {}
    with open("primary/github-package-data.json") as INDEX:
        index = INDEX.read()
        primary_github_data = json.loads(index)

        github_data = primary_github_data["packages"]

    total = len(packages)
    full_pkg_data = []
    for i, pkg in enumerate(packages):

        pkg["is_current"] = pkg["name"] in new_packages

        if pkg["name"] not in github_data:
            print pkg["name"] + " is not in github data"
            pkg["stars"] = 0
            pkg["forks"] = 0
            pkg["watchers"] = 0
            pkg["open_issues"] = 0
            continue
        repo_data = github_data[pkg["name"]]
        pkg["stars"] = repo_data["stargazers_count"]
        pkg["forks"] = repo_data["forks_count"]
        pkg["watchers"] = repo_data["subscribers_count"]
        pkg["open_issues"] = repo_data["open_issues_count"]
        pkg["has_tests"] = repo_data["has_test_dir"]
        pkg["has_examples"] = repo_data["has_examples_dir"]

        description = repo_data["description"]
        if description is None:
            description = ""

        if "deprecated" in description.lower():
            pkg["deprecated"] = True
            redirect = redirect_matcher.search(description)
            if redirect is not None:
                pkg["deprecated_redirect"] = remove_github_prefix(redirect.group(1))
        else:
            pkg["deprecated"] = False

    with open("metrics/current.json", "w") as INDEX:
        INDEX.write(json.dumps({'retrieved':str(now), 'packages':packages}, indent=4))






if __name__ == "__main__":
    # get_elm_package_index()
    get_github_data()
    # extract_metrics()