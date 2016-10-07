import {Component} from "@angular/core"
import {bootstrap} from "@angular/platform-browser-dynamic"
import {HTTP_PROVIDERS, Http, Headers, Response} from "@angular/http"

@Component({
  selector: "todo-nnet-test",
  providers: [HTTP_PROVIDERS],
  template: `
<form class="ui large form segment">

  <div class="field">
    <label for="testData">Test Data:</label>
    <input name="testData" #testData>
  </div>

  <button (click)="postTestData(testData)"
    class="ui positive right floated button">
  Submit</button>
</form>
`
})

class NNetTestDataComponent {
  constructor(private http:Http) {
  }

  postTestData(testData:HTMLInputElement):boolean {
    let header = new Headers( {
      "Content-Type": "text/plain"
    })
    this.http.get("http://localhost:5000/hi")
    return false
  }
}

bootstrap(NNetTestDataComponent, [Http, HTTP_PROVIDERS])