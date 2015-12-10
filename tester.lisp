(load "andrew_huber.lisp")

(setf q (make-instance 'ANDREW-HUBER::my-queue))
(ANDREW-HUBER::enqueue q 'A)
(ANDREW-HUBER::enqueue q 'B)
(ANDREW-HUBER::enqueue q 'C)

(print "(print (ANDREW-HUBER::my-queue-the-queue q))")
(print (ANDREW-HUBER::my-queue-the-queue q))
(terpri)

(print "(print (ANDREW-HUBER::queue-size q))")
(print (ANDREW-HUBER::queue-size q))
(terpri)

(print "(print (ANDREW-HUBER::queue-empty q))")
(print (ANDREW-HUBER::queue-empty q))
(terpri)

(print "(print (ANDREW-HUBER::peek-from-queue q))")
(print (ANDREW-HUBER::peek-from-queue q))
(terpri)

(print "(print (ANDREW-HUBER::dequeue q))")
(print (ANDREW-HUBER::dequeue q))
(terpri)

(print "(print (ANDREW-HUBER::dequeue q))")
(print (ANDREW-HUBER::dequeue q))
(terpri)

(print "(print (ANDREW-HUBER::dequeue q))")
(print (ANDREW-HUBER::dequeue q))
(terpri)

(print "(print (ANDREW-HUBER::my-queue-the-queue q))")
(print (ANDREW-HUBER::my-queue-the-queue q))
(terpri)

(print "(print (ANDREW-HUBER::queue-size q))")
(print (ANDREW-HUBER::queue-size q))
(terpri)

(print "(print (ANDREW-HUBER::queue-empty q))")
(print (ANDREW-HUBER::queue-empty q))
(terpri)