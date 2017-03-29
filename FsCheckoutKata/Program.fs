open Fuchu

type PricingRule = 
    | Standard of string * int
    | Discount of string * int * int * int

let sku = 
    function 
    | Standard(sku, _) -> sku
    | Discount(sku, _, _, _) -> sku

let checkout = 
    let rules = 
        [ Discount("A", 50, 3, 130)
          Discount("B", 30, 2, 45)
          Standard("C", 20)
          Standard("D", 15) ]
        |> Seq.map (fun rule -> rule |> sku, rule)
        |> Map.ofSeq
    [], rules

let price rule qty = 
    match rule with
    | Standard(sku, price) -> price * qty
    | Discount(sku, price, dscQty, dscPrice) -> 
        let p1 = dscPrice * (qty / dscQty)
        let p2 = price * (qty % dscQty)
        p1 + p2

let scan sku (cart, rules) = (sku :: cart), rules

let total (cart, rules) = 
    cart
    |> Seq.groupBy id
    |> Seq.sumBy (fun (sku, list) -> price (Map.find sku rules) (Seq.length list))

[<Tests>]
let tests = 
    testList "checkout kata" [ testList "sku price" [ testCase "item A" <| fun _ -> 
                                                          Assert.Equal("unit price", 50, 
                                                                       checkout
                                                                       |> scan "A"
                                                                       |> total)
                                                      testCase "item B" <| fun _ -> 
                                                          Assert.Equal("unit price", 30, 
                                                                       checkout
                                                                       |> scan "B"
                                                                       |> total)
                                                      testCase "item C" <| fun _ -> 
                                                          Assert.Equal("unit price", 20, 
                                                                       checkout
                                                                       |> scan "C"
                                                                       |> total)
                                                      testCase "item D" <| fun _ -> 
                                                          Assert.Equal("unit price", 15, 
                                                                       checkout
                                                                       |> scan "D"
                                                                       |> total) ]
                               testCase "multiple items" <| fun _ -> 
                                   Assert.Equal("A + B + C + D = 115", 115, 
                                                checkout
                                                |> scan "A"
                                                |> scan "B"
                                                |> scan "C"
                                                |> scan "D"
                                                |> total)
                               testList "discounts" [ testCase "A discount" <| fun _ -> 
                                                          Assert.Equal("A + A + A = 130", 130, 
                                                                       checkout
                                                                       |> scan "A"
                                                                       |> scan "A"
                                                                       |> scan "A"
                                                                       |> total)
                                                      testCase "A and B discount and a bit" <| fun _ -> 
                                                          Assert.Equal("A + A + B + C + B + A + A = 245", 245, 
                                                                       checkout
                                                                       |> scan "A"
                                                                       |> scan "A"
                                                                       |> scan "B"
                                                                       |> scan "C"
                                                                       |> scan "B"
                                                                       |> scan "A"
                                                                       |> scan "A"
                                                                       |> total) ]
                               testCase "left fold" <| fun _ ->
                                   let calc basket sku = scan (sku.ToString()) basket
                                   let price =
                                        "ABCD"
                                        |> Seq.fold calc checkout
                                        |> total
                                   Assert.Equal("A + B + C + D = 115", 115, price) ]

[<EntryPoint>]
let main args = defaultMainThisAssembly args