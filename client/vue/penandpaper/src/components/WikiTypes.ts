export class Attribute {
  predicate: string = ''
  value: string = ''
  isInteresting: boolean = false
  isInheritable: boolean = false
  isDate: boolean = false

  constructor (predicate: string, value: string, isInteresting: boolean, isInheritable: boolean, isDate: boolean) {
    this.predicate = predicate
    this.value = value
    this.isInteresting = isInteresting
    this.isInheritable = isInheritable
    this.isDate = isDate
  }
}

export class DisplayedAttribute {
  idx: number
  data: Attribute
  constructor (idx: number, data: Attribute) {
    this.idx = idx
    this.data = data
  }
}
