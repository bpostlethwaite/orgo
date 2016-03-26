package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path"

	"github.com/google/go-github/github"
	"golang.org/x/oauth2"
)

func main() {

	state := "open"
	if len(os.Args) == 2 && os.Args[1] == "closed" {
		state = "closed"
	}

	confpath := path.Join(os.Getenv("HOME"), ".plotbot")

	token, err := getToken(confpath)

	ts := oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: token},
	)
	tc := oauth2.NewClient(oauth2.NoContext, ts)

	client := github.NewClient(tc)

	// list all repositories for the authenticated user
	opts := github.IssueListByRepoOptions{
		Assignee: "bpostlethwaite",
		State:    state,
	}
	issues, _, err := client.Issues.ListByRepo("Plotly", "streambed", &opts)

	if err != nil {
		log.Fatal(err)
	}

	todos := make([]Todo, len(issues))
	for i, issue := range issues {
		todos[i] = Todo{
			Title: *issue.Title,
			URL:   *issue.HTMLURL,
		}
	}

	json, err := json.Marshal(todos)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(string(json))
}

type Todo struct {
	Title string
	URL   string
}

type Config struct {
	Github *GHConf
}

type GHConf struct {
	Authtoken string
}

func getToken(confPath string) (string, error) {

	content, err := ioutil.ReadFile(confPath)
	if err != nil {
		return "", fmt.Errorf("problem reading configuration %s", confPath)
	}

	conf := Config{}

	err = json.Unmarshal(content, &conf)
	if err != nil {
		return "", fmt.Errorf("problem reading configuration %s", confPath)
	}

	if conf.Github == nil {
		return "", fmt.Errorf("Missing GitHub field in %s", confPath)
	}

	if conf.Github.Authtoken == "" {
		return "", fmt.Errorf("Missing github.authtoken field in %s", confPath)
	}

	return conf.Github.Authtoken, nil
}
