import {Component, Input, Output, EventEmitter} from "@angular/core"
import {bootstrap} from "@angular/platform-browser-dynamic"

@Component({
  selector: "product-row",
  template: `
<div class="ui items">
{{ product.sku }}
</div>
`
})
class ProductRowComponent {
  @Input() product: Product
}

@Component({
  selector: "product-list",
  directives: [ProductRowComponent],
  template: `
<div class="ui items" *ngFor="let prod of products">
  <product-row
    [product]="prod"
    (click)="clicked(prod)"
    [class.selected] = "isSelected(prod)">
  </product-row>
</div>
`
})
class ProductListComponent {

  @Input() products: Product[]
  @Output() onProductSelected: EventEmitter<Product>

  currentProduct: Product

  clicked(product: Product):void {
    this.currentProduct = product
    this.onProductSelected.emit(product)
  }

  isSelected(product: Product): boolean {
    if (!product || !this.currentProduct) {
      return false
    }
    return product.sku === this.currentProduct.sku
  }

  constructor() {
    this.onProductSelected = new EventEmitter()
  }
}

@Component({
  selector: "inventory-app",
  directives: [ProductListComponent],
  template: `
<div class="inventory-app">
  <product-list
    [products]="_products"
    (onProductSelected)="productWasSelected($event)">
  </product-list>
</div>
`
})
class InventoryComponent {
  _products: Product[]

  productWasSelected(product: Product): void {
    console.log("Product clicked: ", product)
  }

  constructor() {
    this._products = [
      new Product("shoes", "Running Shoes", "res/imgs/products/running-shoes.jpg", [ "Men", "Shoes", "Running" ], 109.99),
      new Product("jacket", "Blue Jacket", "res/imgs/products/jacket.jpg", [ "Women", "Jackets" ], 129.99),
      new Product("hat", "Black Jacket", "res/imgs/products/hat.jpg", [ "Men", "Hats" ], 29.99)
    ]
  }
}


class Product {
  constructor(
    public sku: string,
    public name: string,
    public imgUrl: string,
    public department: string[],
    public price: number) {
  }
}

bootstrap(InventoryComponent)