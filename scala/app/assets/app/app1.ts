import {bootstrap} from "@angular/platform-browser-dynamic"
import {Component} from "@angular/core"

@Component({
  selector: "todo-world",
  template: `
    <div> 8 Hello {{ name }}! </div>
    <ul>
     <li *ngFor="let n of names">Hi {{ n }}</li>
    </ul>
  `

})

export class HelloWorldComponent {
  name: string
  names: string[]

  constructor() {
    this.name = "filip"
    this.names = [ "Alice", "Tuva", "Joel" ]
  }
}

bootstrap(HelloWorldComponent)