package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"os"
	"os/user"
	"path"
	"path/filepath"
	"time"

	"gopkg.in/alecthomas/kingpin.v2"

	"google.golang.org/api/calendar/v3"

	"github.com/google/go-github/github"
	"golang.org/x/net/context"
	"golang.org/x/oauth2"
	"golang.org/x/oauth2/google"
)

var (
	app = kingpin.New("orgo", "get data out of your favorite services for Org-Mode TODOS")

	gh        = app.Command("github", "target github backend")
	assignee  = gh.Arg("assignee", "issue assignee").Required().String()
	repoOwner = gh.Arg("repoowner", "owner or namespace of repo").Required().String()
	repoName  = gh.Arg("reponame", "name of repo").Required().String()
	state     = gh.Flag("state", "issue state").Default("open").Enum("open", "closed")

	gcal = app.Command("gcal", "target google calendar backend")
)

type Todo struct {
	Title       string
	URL         string
	Date        string
	Description string
}

type Config struct {
	Github         *GHConf
	GoogleCalendar *GCal `json:"google-calendar"`
}

type GHConf struct {
	Authtoken string
}

type GCal struct {
	Installed map[string]interface{}
}

func main() {
	var todos []Todo
	var err error

	confPath := path.Join(os.Getenv("HOME"), ".orgo")
	content, err := ioutil.ReadFile(confPath)
	if err != nil {
		log.Fatal(fmt.Errorf("problem reading configuration %s", confPath))
	}

	conf := Config{}
	err = json.Unmarshal(content, &conf)
	if err != nil {
		log.Fatal(fmt.Errorf("problem reading configuration %s", confPath))
	}

	switch kingpin.MustParse(app.Parse(os.Args[1:])) {
	case gh.FullCommand():
		todos, err = getGithubTodos(conf)
	case gcal.FullCommand():
		todos, err = getGCalTodos(conf)
	}

	// TODO improve error handling
	if err != nil {
		log.Fatal(err)
	}

	json, err := json.Marshal(todos)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(string(json))
}

func getGithubTodos(conf Config) ([]Todo, error) {

	if conf.Github == nil {
		return nil, fmt.Errorf("Missing GitHub field in config")
	}

	if conf.Github.Authtoken == "" {
		return nil, fmt.Errorf("Missing github.authtoken field in config")
	}

	ts := oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: conf.Github.Authtoken},
	)
	tc := oauth2.NewClient(oauth2.NoContext, ts)

	client := github.NewClient(tc)

	// list all repositories for the authenticated user
	opts := github.IssueListByRepoOptions{
		Assignee: *assignee,
		State:    *state,
	}
	issues, _, err := client.Issues.ListByRepo(*repoOwner, *repoName, &opts)

	if err != nil {
		return nil, err
	}

	todos := make([]Todo, len(issues))
	for i, issue := range issues {
		todos[i] = Todo{
			Title: *issue.Title,
			URL:   *issue.HTMLURL,
		}
	}

	return todos, nil
}

// getClient uses a Context and Config to retrieve a Token
// then generate a Client. It returns the generated Client.
func getClient(ctx context.Context, config *oauth2.Config) *http.Client {
	cacheFile, err := tokenCacheFile()
	if err != nil {
		log.Fatalf("Unable to get path to cached credential file. %v", err)
	}
	tok, err := tokenFromFile(cacheFile)
	if err != nil {
		tok = getTokenFromWeb(config)
		saveToken(cacheFile, tok)
	}
	return config.Client(ctx, tok)
}

// getTokenFromWeb uses Config to request a Token.
// It returns the retrieved Token.

func getTokenFromWeb(config *oauth2.Config) *oauth2.Token {
	authURL := config.AuthCodeURL("state-token", oauth2.AccessTypeOffline)
	fmt.Printf("Go to the following link in your browser then type the "+
		"authorization code: \n%v\n", authURL)

	var code string
	if _, err := fmt.Scan(&code); err != nil {
		log.Fatalf("Unable to read authorization code %v", err)
	}

	tok, err := config.Exchange(oauth2.NoContext, code)
	if err != nil {
		log.Fatalf("Unable to retrieve token from web %v", err)
	}
	return tok
}

// tokenCacheFile generates credential file path/filename.
// It returns the generated credential path/filename.
func tokenCacheFile() (string, error) {
	usr, err := user.Current()
	if err != nil {
		return "", err
	}
	tokenCacheDir := filepath.Join(usr.HomeDir, ".credentials")
	os.MkdirAll(tokenCacheDir, 0700)
	return filepath.Join(tokenCacheDir,
		url.QueryEscape("calendar-go-quickstart.json")), err
}

// tokenFromFile retrieves a Token from a given file path.
// It returns the retrieved Token and any read error encountered.
func tokenFromFile(file string) (*oauth2.Token, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	t := &oauth2.Token{}
	err = json.NewDecoder(f).Decode(t)
	defer f.Close()
	return t, err
}

// saveToken uses a file path to create a file and store the
// token in it.
func saveToken(file string, token *oauth2.Token) {
	fmt.Printf("Saving credential file to: %s\n", file)
	f, err := os.Create(file)
	if err != nil {
		log.Fatalf("Unable to cache oauth token: %v", err)
	}
	defer f.Close()
	json.NewEncoder(f).Encode(token)
}

func getGCalTodos(conf Config) ([]Todo, error) {
	ctx := context.Background()

	b, err := json.Marshal(conf.GoogleCalendar)
	if err != nil {
		return nil, err
	}

	// If modifying these scopes, delete your previously saved credentials
	// at ~/.credentials/calendar-go-quickstart.json
	config, err := google.ConfigFromJSON(b, calendar.CalendarReadonlyScope)
	if err != nil {
		return nil, err
	}
	client := getClient(ctx, config)

	srv, err := calendar.New(client)
	if err != nil {
		return nil, err
	}

	tstart := time.Now().Format(time.RFC3339)
	tend := time.Now().AddDate(0, 0, 60).Format(time.RFC3339)
	events, err := srv.Events.List("primary").ShowDeleted(false).
		TimeMin(tstart).TimeMax(tend).Do()
	if err != nil {
		return nil, err
	}

	todos := make([]Todo, len(events.Items))
	for i, ev := range events.Items {

		var when string
		// If the DateTime is an empty string the Event is an all-day Event.
		// So only Date is available.
		if ev.Start.DateTime != "" {
			when = ev.Start.DateTime
		} else {
			when = ev.Start.Date
		}

		todos[i] = Todo{
			Title:       ev.Summary,
			URL:         ev.HtmlLink,
			Date:        when,
			Description: ev.Description,
		}
	}

	return todos, nil
}
