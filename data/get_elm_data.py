import requests
import json
import time
import datetime
import re
import pprint
import requests.utils

now = datetime.datetime.now()

all_packages = "http://package.elm-lang.org/all-packages"
packages_for_seventeen = "http://package.elm-lang.org/new-packages"
github_json = "https://api.github.com/repos/"
github_releases = "https://api.github.com/repos/{package}/releases"
test_dir = "https://api.github.com/repos/{package}/contents/test?ref=master"
tests_dir = "https://api.github.com/repos/{package}/contents/tests?ref=master"
examples_dir = "https://api.github.com/repos/{package}/contents/examples?ref=master"
elm_package_file = "https://raw.githubusercontent.com/{package}/master/elm-package.json?ref=master"
download_file_location = "https://github.com/{package}/archive/master.zip"

redirect_matcher = re.compile("(?:elm-lang.org/|github.com/)?(\S+/[^/\.\s]+)")


def wait_for_reset_if_necessary(headers):
    limit = headers.get("X-RateLimit-Limit", None)
    remain = headers.get("X-RateLimit-Remaining", None)
    resetAt = headers.get("X-RateLimit-Reset",None)


    if remain is not None and int(remain) % 100 == 0:
        print str(remain)  + " github requests remaining until pause for reset."

    if resetAt is not None:
        if remain is not None and int(remain) == 0:
            now_epoch = time.time()
            waitFor = int(resetAt) - now_epoch
            print "RESET REQUIRED: " + str(resetAt)
            print "limit is at " + str(limit)
            print "remaining: " + str(remain)
            print "waiting for " + str(waitFor) + " seconds for limits to reset"
            time.sleep(waitFor + 10)



def wait_for_reset(api_type):
    rate_limit = get_rate_limit()
    if api_type == "search":
        limit = rate_limit['resources']['search']['limit']
        remain = rate_limit['resources']['search']['remaining']
        resetAt = rate_limit['resources']['search']['reset']
    else:
        raise Exception

    if resetAt is not None:
        if remain is not None and int(remain) == 0:
            now_epoch = time.time()
            waitFor = int(resetAt) - now_epoch
            print "RESET REQUIRED: " + str(resetAt)
            print "limit is at " + str(limit)
            print "remaining: " + str(remain)
            print "waiting for " + str(waitFor + 10) + " seconds for limits to reset"
            time.sleep(waitFor + 10)


def download_file(url, local_filename):
    # local_filename = url.split('/')[-1]
    # NOTE the stream=True parameter
    r = requests.get(url, stream=True)
    with open(local_filename, 'wb') as f:
        for chunk in r.iter_content(chunk_size=1024): 
            if chunk: # filter out keep-alive new chunks
                f.write(chunk)
                #f.flush() commented by recommendation from J.F.Sebastian
    wait_for_reset_if_necessary(r.headers)
    return local_filename


def download_elm_package_zip():

    credentials = get_github_credentials()

    packages = []
    with open("primary/package-index.json") as INDEX:
        index = INDEX.read()
        packages = json.loads(index)

    print "retrieving github data"
    if not os.path.exists("raw"):
        os.makedirs("raw")

    total = len(packages)
    full_pkg_data = {}
    for i, pkg in enumerate(packages):
        print str(i) + "/" + str(total) + " - Retriving github data for: " + str(pkg["name"])

        download_file(download_file_location.format(package=pkg["name"]), "raw/{kebab}.zip".format(kebab=pkg["name"].replace("/", "@")))

        time.sleep(0.1)

def get_github_credentials():
    credentials = None
    with open('credentials/github.json') as GITHUB:
        credentials = json.loads(GITHUB.read())
    if credentials is None:
        raise "No github credentials!"
    return credentials


def github_get(url, credentials):
    data = requests.get(url, params=credentials)
    result = None
    if data.status_code < 300 and data.status_code >= 200:
        result = json.loads(data.content)
    wait_for_reset_if_necessary(data.headers)
    return result

def get_github_data(projects):

    credentials = get_github_credentials()
    
    print "retrieving github data"
    total = len(projects)
    full_pkg_data = {}
    for i, pkg in enumerate(projects):
        print str(i+1) + "/" + str(total) + " - Retriving github data for: " + str(pkg["name"])
        pkg_data = requests.get(github_json + pkg["name"], params=credentials)

        github_data = {}
        if pkg_data.status_code < 300 and pkg_data.status_code >= 200:
            github_data = json.loads(pkg_data.content)
        else:
            print "failed retrieval"
        wait_for_reset_if_necessary(pkg_data.headers)

        if github_data:

            # we removed these checks because we dont use them.
            github_data["has_test_dir"] = False
            github_data["has_examples_dir"] = False


            # Retrieve Elm Package Info
            for x in range(5):
                try:
                    elm_package = requests.get(elm_package_file.format(package=pkg["name"]), params=credentials)
                    break
                except:
                    print "File retrieval timed out, trying in 5 seconds."
                    time.sleep(5)

            if elm_package.status_code < 300 and elm_package.status_code >= 200:
                try:
                    github_data["elm_package"] = json.loads(elm_package.content)
                except ValueError:
                    print "Error parsing elm-package.json for " + pkg["name"]
                    github_data["elm_package"] = None

            else:
                github_data["elm_package"] = None

            # test = requests.get('https://api.github.com/repos/reactiveui/ReactiveUI/releases',  params=credentials)
            # print test.content

            # # retrieve releases information
            # releases = requests.get(github_releases.format(package=pkg["name"]), params=credentials)
            # print github_releases.format(package=pkg["name"])
            # print str(releases)
            # print str(releases.headers)
            # release_data = {}
            # if releases.status_code < 300 and releases.status_code >= 200:
            #     print releases.content
            #     release_data = json.loads(releases.content)
            # else:
            #     print "failed retrieval"
            # wait_for_reset_if_necessary(releases.headers)
            # github_data["releases"] = release_data
            full_pkg_data[pkg["name"]] = github_data
            wait_for_reset_if_necessary(elm_package.headers)



    # with open("primary/github-package-data.json", "w") as INDEX:
    #     INDEX.write(json.dumps({'retrieved':str(now), 'packages':full_pkg_data}, indent=4))


    return {'retrieved':str(now), 'packages':full_pkg_data}
   
    

def remove_prefix(text, prefix):
    return text[text.startswith(prefix) and len(prefix):]

def remove_github_prefix(text):
    text = remove_prefix(text, "http://")
    text = remove_prefix(text, "https://")
    text = remove_prefix(text, "github.com/")
    text = remove_prefix(text, "package.elm-lang.org/")
    return text



    


def extract_metrics(packages, full_github_data):
    # packages = []
    # with open("primary/package-index.json") as INDEX:
    #     index = INDEX.read()
    #     packages = json.loads(index)

    # new_packages = []
    # with open("primary/new-packages.json") as INDEX:
    #     index = INDEX.read()
    #     new_packages = json.loads(index)

    # github_data = {}
    # with open("primary/github-package-data.json") as INDEX:
    #     index = INDEX.read()
    #     primary_github_data = json.loads(index)

    #     github_data = primary_github_data["packages"]

    github_data = full_github_data["packages"]


    reverse_dep_count = {}

    total = len(packages)
    full_pkg_data = []
    for i, pkg in enumerate(packages):

        # Defaults
        pkg["is_current"] = "is_current" in pkg
        pkg["stars"] = 0
        pkg["forks"] = 0
        pkg["watchers"] = 0
        pkg["open_issues"] = 0
        pkg["has_tests"] = False
        pkg["has_examples"] = False
        pkg["license"] = "unknown"
        pkg["is_project"] = pkg["project_type"] == "project"
        pkg["deprecated"] = False
        pkg["deprecation_redirect"] = None
        pkg["no_data"] = True

        if pkg["name"] not in github_data:
            print pkg["name"] + " is not in github data"
            continue

        repo_data = github_data[pkg["name"]]
        pkg["no_data"] = False
        pkg["stars"] = repo_data["stargazers_count"]
        pkg["forks"] = repo_data["forks_count"]
        pkg["watchers"] = repo_data["subscribers_count"]
        pkg["open_issues"] = repo_data["open_issues_count"]
        pkg["has_tests"] = repo_data["has_test_dir"]
        pkg["has_examples"] = repo_data["has_examples_dir"]
        if "elm_package" in repo_data and repo_data["elm_package"] is not None:
            pkg["license"] = repo_data["elm_package"].get("license", "unknown")

        description = repo_data["description"]
        if description is None:
            description = ""


        if "summary" not in pkg or pkg["summary"] is None:
            pkg["summary"] = description

        # check github summary for deprecation
        if "deprecated" in description.lower():
            pkg["deprecated"] = True
            redirect = redirect_matcher.search(description)
            if redirect is not None:
                pkg["deprecation_redirect"] = remove_github_prefix(redirect.group(1))

        # check elm-package summary for deprecations
        if pkg["summary"] is not None and "deprecated" in pkg["summary"].lower():
            pkg["deprecated"] = True
            redirect = redirect_matcher.search(pkg["summary"])
            if redirect is not None:
                pkg["deprecation_redirect"] = remove_github_prefix(redirect.group(1))

        if repo_data["elm_package"] is not None:
            for key, versions in repo_data["elm_package"]["dependencies"].items():
                if key in reverse_dep_count:
                    if versions in reverse_dep_count[key]:
                        if pkg["name"] not in reverse_dep_count[key][versions]:
                            reverse_dep_count[key][versions] = reverse_dep_count[key][versions] + [pkg["name"]]
                    else:
                        reverse_dep_count[key][versions] = [pkg["name"]]
                else:
                    reverse_dep_count[key] = {}
                    reverse_dep_count[key][versions] = [pkg["name"]]


    if not os.path.exists("metrics"):
        os.makedirs("metrics")
 

    with open("metrics/reverse_deps.json", "w") as INDEX:
        INDEX.write(json.dumps(reverse_dep_count, indent=None))


    with open("metrics/current.json", "w") as INDEX:
        INDEX.write(json.dumps({'retrieved':str(now), 'packages':packages}, indent=None))


def render_template():
    with open("metrics/current.json") as PACKAGES:
        with open("metrics/reverse_deps.json") as DEPS:
            with open("template.html") as TEMPLATE:
                with open("../html/index.html", "w") as TARGET:
                    template = TEMPLATE.read()
                    TARGET.write(template.format(package_data=PACKAGES.read(), reverse_deps=DEPS.read()))


def get_elm_package_index():
    print "retrieving elm package index"


    current_pkgs = []
    all_pkgs = requests.get(packages_for_seventeen)
    if all_pkgs.status_code < 300 and all_pkgs.status_code >= 200:
        current_pkgs = json.loads(all_pkgs.content)
    else:
        print "FAILURE"
        raise Exception

    all_pkgs_response = requests.get(all_packages)
    if all_pkgs_response.status_code < 300 and all_pkgs_response.status_code >= 200:
        all_pkgs = json.loads(all_pkgs_response.content)

    for pkg in all_pkgs:
        if pkg["name"] in current_pkgs:
            pkg[u"is_current"] = True
        pkg[u"project_type"] = u"package"
    return all_pkgs
            

def get_rate_limit():
    credentials = get_github_credentials()
    limit = requests.get("https://api.github.com/rate_limit", params=credentials)
    return json.loads(limit.content)


def get_top_elm_repos():
    credentials = get_github_credentials()

    packages = []
    with open("primary/package-index.json") as INDEX:
        index = INDEX.read()
        packages = json.loads(index)
    package_names = [pkg["name"] for pkg in packages]

    search = "https://api.github.com/search/repositories"
    search_params = {'q':'language:elm', 'sort':'stars', 'order':'desc'}
    search_params.update(credentials)
    retrieved = 0
    actual_count = 0
    projects = []

    print "searching for elm projects..."
    while retrieved < 10000:
        wait_for_reset("search")
        projects_response = requests.get(search, params=search_params)

        # pprint.pprint(json.loads(projects_response.content))
        # break
        if projects_response.status_code < 300 and projects_response.status_code >= 200:
            project_data = json.loads(projects_response.content)

            actual_count = actual_count + len(project_data["items"])
            
            only_nonpackages = [proj for proj in project_data["items"] if proj["full_name"] not in package_names ]
            projects.extend([ { u"name":proj["full_name"]
                              , u"summary":proj["description"]
                              , u"project_type":u"project"
                              , u"versions":None
                              } for proj in only_nonpackages
                            ])
            retrieved = len(projects)
            print "retrieved " + str(retrieved) + " out of (" + str(actual_count) + "/" + str(project_data['total_count']) + ")"
            if actual_count >= project_data['total_count']:
                break
        else:
            print "error retrieving projects"
            continue
        try:
            search = [link for link in requests.utils.parse_header_links(projects_response.headers['Link']) if link['rel'] == 'next'][0]["url"]
            search_params = credentials
        except IndexError:
            print "Unable to find link to next page, breaking"
            break
        

    return projects


def get_whitelist():
    whitelisted = []
    with open("whitelist") as WHITELIST: 
        for name in WHITELIST.read().split("\n"):
            if name != "" and not name.isspace():
                whitelisted.append({ u"name":name
                                   , u"project_type":u"project"
                                   , u"versions":None
                                   }
                                  )
    return whitelisted




if __name__ == "__main__":
    # pprint.pprint(get_rate_limit())

    # download_elm_package_zip()
    projects = get_top_elm_repos()
    packages = get_elm_package_index()
    whitelist = get_whitelist()

    everything = packages + projects + whitelist

    github_data = get_github_data(everything)
    # # pprint.pprint(github_data)
    extract_metrics(everything, github_data)
    render_template()



