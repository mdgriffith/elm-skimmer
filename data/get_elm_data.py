import requests
import json
import time
import datetime

all_packages = "http://package.elm-lang.org/all-packages"

packages_for_seventeen = "http://package.elm-lang.org/new-packages"

github_json = "https://api.github.com/repos/"

now = datetime.datetime.now()



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
            print "waiting for " + str(waitFor) + "seconds for limits to reset"
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
    full_pkg_data = []
    for i, pkg in enumerate(packages):
        print str(i) + "/" + str(total) + " - Retriving github data for: " + str(pkg["name"])
        pkg_data = requests.get(github_json + pkg["name"], params=credentials)

        wait_for_reset_if_necessary(pkg_data.headers)

        if pkg_data.status_code < 300 and pkg_data.status_code >= 200:
            full_pkg_data.append((pkg["name"], json.loads(pkg_data.content)))
        else:
            print "failed retrieval"

        time.sleep(0.1)


    with open("primary/github-package-data.json", "w") as INDEX:
        INDEX.write(json.dumps({'retrieved':str(now), 'packages':full_pkg_data}, indent=4))
    



if __name__ == "__main__":
    get_elm_package_index()
    get_github_data()