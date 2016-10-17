import {Component} from "@angular/core"
import {FORM_DIRECTIVES,REACTIVE_FORM_DIRECTIVES,FormBuilder,FormGroup} from "@angular/forms"
import {bootstrap} from "@angular/platform-browser-dynamic"

@Component({
  selector: "demo-form",
  directives: [ FORM_DIRECTIVES, REACTIVE_FORM_DIRECTIVES ],
  template: `
<div class="ui raised segment">
  <h2 class="ui header">Demo form: Sku</h2>
  <form [formGroup]="myForm" (ngSubmit)="onSubmit(myForm.value)" class="ui form">
    <div class="field">
      <label for="skuIput">SKU</label>
      <input type="text" id="skuInput" placeholder="SKU" name="sku" [formControl]="myForm.controls['sku']">
    </div>
    <button type="submit" class="ui button">Submit</button>
  </form>
</div>
`
})

export class DemoFormComponent {
  myForm: FormGroup

  constructor(fBuilder: FormBuilder) {
    this.myForm = fBuilder.group(
      { "sku": ["ABC123"] }
    )
  }

  onSubmit(value: string):void {
    console.log("you submitted value: ", value)
  }
}

bootstrap(DemoFormComponent, [ FORM_DIRECTIVES, REACTIVE_FORM_DIRECTIVES ])