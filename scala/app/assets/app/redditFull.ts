import {bootstrap} from "@angular/platform-browser-dynamic"
import {Component,Input,HostBinding} from "@angular/core"

class Article {
  title:string
  link:string
  votes:number

  constructor(title:string, link:string, votes?:number) {
    this.title = title
    this.link = link
    this.votes = votes || 0
  }

  domain():string {
    try {
      const link:string = this.link.split("//")[1]
      return link.split("/")[0]
    } catch(err) {
      return null
    }
  }

  voteUp():void {
    this.votes += 1
  }

  voteDown():void {
    this.votes -= 1
  }
}

@Component({
  selector: "todo-reddit-article",
  // host: {
  //  class: "row"
  // },
  template: `
<div class="four wide column center aligned votes">
  <div class="ui statistic">
    <div class="value">{{ article.votes }}</div>
    <div class="label">Points</div>
  </div>
</div>
<div class="twelve wide column">
  <a class="ui large header" href="{{ article.link }}">{{ article.title }}</a>
  <div class="meta">({{ article.domain() }})</div>
  <ul class="ui big horizontal list votes">
    <li class="item">
      <a href (click)="voteUp()">
        <i class="arrow up icon"></i>
        upvote
      </a>
    </li>
    <li class="item">
      <a href (click)="voteDown()">
        <i class="arrow down icon"></i>
        downvote
      </a>
    </li>
  </ul>
</div>
`
})

class ArticleComponent {

  @Input() article:Article
  @HostBinding("class") rowClass="row"

  voteUp(): boolean {
    this.article.voteUp()
    return false
  }

  voteDown(): boolean {
    this.article.voteDown()
    return false
  }
}

@Component({
  selector: "todo-reddit",
  directives: [ArticleComponent],
  template: `
<form class="ui large form segment">
  <h3 class="ui header">Add a link</h3>
  <div class="field">
    <label for="title">Title:</label>
    <input name="title" #newTitle>
  </div>
  <div class="field">
    <label for="link">Link:</label>
    <input name="link" #newLink>
  </div>

  <button (click)="addArticle(newTitle, newLink)" class="ui positive right floated button">
    Submit link
  </button>
</form>

<div class="ui grid posts">
  <todo-reddit-article
    *ngFor="let article of sortedArticles()"
    [article]="article"></todo-reddit-article>
</div>
`
})

class RedditComponent {
  articles:Article[]

  constructor() {
    this.articles = [
      new Article("ang 2", "http://angular.io", 3),
      new Article("ms", "http://microsoft.com", 3),
      new Article("test", "http://article.com/abs", 1)
    ]
  }

  addArticle(title:HTMLInputElement, link:HTMLInputElement):void {
    console.log(`adding article (title: ${title.value}, link: ${link.value}`)

    this.articles.push(new Article(title.value, link.value, 0))
    title.value = ""
    link.value = ""
  }

  sortedArticles():Array<Article> {
    return this.articles.sort((a1:Article, a2:Article) => a2.votes - a1.votes)
  }
}

bootstrap(RedditComponent)