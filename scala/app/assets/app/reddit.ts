import {Component} from "@angular/core"
import {bootstrap} from "@angular/platform-browser-dynamic"

@Component({
  selector: "todo-reddit",
  template: `
<form class="ui large form segment">
  <h3 class="ui header">Add a Link</h3>

  <div class="field">
    <label for="title">Title:</label>
    <input name="title" #newTitle>
  </div>

  <div class="field">
    <label for="link">Link:</label>
    <input name="link" #newLink>
  </div>

  <button (click)="addArticle(newTitle, newLink)"
    class="ui positive right floated button">
  Submit</button>
</form>
`
})

class RedditComponent {


  addArticle(title:HTMLInputElement, link:HTMLInputElement):void {
    console.log(`Adding article, title: ${title.value} and link: ${link.value}`)
  }
}

bootstrap(RedditComponent)