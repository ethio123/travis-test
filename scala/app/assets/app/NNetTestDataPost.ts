import {Component} from "@angular/core"
import {bootstrap} from "@angular/platform-browser-dynamic"
import {HTTP_PROVIDERS, Http, Headers, Response, RequestOptions} from "@angular/http"

@Component({
  selector: "todo-nnet-test",
  providers: [HTTP_PROVIDERS],
  template: `
<form class="ui large form segment">

  <div class="field">
    <label for="testData">Test Data:</label>
    <input name="testData" #testData>
  </div>

  <div class="field">
    <input name="testResult" #testResult>
  </div>

  <button (click)="postTestData(testData, testResult)"
    class="ui positive right floated button">
  Submit</button>
</form>
`
})

class NNetTestDataComponent {
  constructor(private http:Http) {
  }

  postTestData(testData:HTMLInputElement, testResult:HTMLInputElement):boolean {
    let testUrl = "eval"
    let headers = new Headers( {
      "Content-Type": "text/plain"
    })
    let body = testData.value
    let options = new RequestOptions( { headers: headers })

    this.http.post(testUrl, body, options)
      .subscribe((res:Response) => {
        console.log(res)
        testResult.value = res.text()
      })

    return false
  }
}

bootstrap(NNetTestDataComponent, [Http, HTTP_PROVIDERS])