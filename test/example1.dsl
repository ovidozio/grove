canvas 100 100

let sum(a)(b) = "Sum from " ^ a ^ " to " ^ b

node A at 10 90 label "pi1(U1)"
node B at 90 90 label "pi1(U2)"
node C at 50 50 label {sum(1)(n)}

arrow A -> C label "i1"
arrow B -> C label "i2"
arrow C -> D dashed label "equiv"

