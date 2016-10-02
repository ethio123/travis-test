import {bootstrap} from "@angular/platform-browser-dynamic"
import {Component} from "@angular/core"

@Component({
  selector: "todo-world",
  template: `
    <div> 8 Hello world 9 </div>
  `

})

export class HelloWorldComponent {
}

bootstrap(HelloWorldComponent)