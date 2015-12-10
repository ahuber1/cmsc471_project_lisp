(load "andrew_huber.lisp")

(setf s (make-instance 'ANDREW-HUBER::my-stack))

(ANDREW-HUBER::push-to-stack s 'A)
(ANDREW-HUBER::push-to-stack s 'B)
(ANDREW-HUBER::push-to-stack s 'C)

(setf c (ANDREW-HUBER::copy-stack s))

(ANDREW-HUBER::stack-size s)
(ANDREW-HUBER::is-empty s)
(ANDREW-HUBER::peek-from-stack s)
(ANDREW-HUBER::pop-from-stack s)
(ANDREW-HUBER::pop-from-stack s)
(ANDREW-HUBER::pop-from-stack s)
(ANDREW-HUBER::stack-size s)
(ANDREW-HUBER::is-empty s)
(ANDREW-HUBER::pop-from-stack s)

(print (ANDREW-HUBER::my-stack-the-stack c))
(print (ANDREW-HUBER::xth-last-item-of-stack c 1))
(print (ANDREW-HUBER::xth-last-item-of-stack c 2))
(print (ANDREW-HUBER::xth-last-item-of-stack c 3))