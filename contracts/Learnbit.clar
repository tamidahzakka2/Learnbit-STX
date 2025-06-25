(define-trait course-trait
  (
    (get-course-info (uint) (response {title: (string-ascii 100), price: uint, instructor: principal} uint))
  )
)

(define-non-fungible-token course-certificate uint)

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_COURSE_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u102))
(define-constant ERR_ALREADY_ENROLLED (err u103))
(define-constant ERR_NOT_ENROLLED (err u104))
(define-constant ERR_COURSE_NOT_COMPLETED (err u105))
(define-constant ERR_CERTIFICATE_ALREADY_ISSUED (err u106))
(define-constant ERR_INVALID_COURSE (err u107))
(define-constant PLATFORM_FEE_PERCENT u5)

(define-data-var next-course-id uint u1)
(define-data-var next-certificate-id uint u1)
(define-data-var platform-earnings uint u0)

(define-map courses
  uint
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    instructor: principal,
    price: uint,
    duration-blocks: uint,
    is-active: bool,
    total-enrolled: uint,
    created-at: uint
  }
)

(define-map enrollments
  {course-id: uint, student: principal}
  {
    enrolled-at: uint,
    completed-at: (optional uint),
    certificate-id: (optional uint),
    payment-amount: uint
  }
)

(define-map instructor-earnings
  principal
  uint
)

(define-map course-reviews
  {course-id: uint, student: principal}
  {
    rating: uint,
    review: (string-ascii 200),
    created-at: uint
  }
)

(define-map certificates
  uint
  {
    course-id: uint,
    student: principal,
    instructor: principal,
    issued-at: uint,
    course-title: (string-ascii 100)
  }
)

(define-public (create-course (title (string-ascii 100)) (description (string-ascii 500)) (price uint) (duration-blocks uint))
  (let
    (
      (course-id (var-get next-course-id))
      (current-block stacks-block-height)
    )
    (map-set courses course-id
      {
        title: title,
        description: description,
        instructor: tx-sender,
        price: price,
        duration-blocks: duration-blocks,
        is-active: true,
        total-enrolled: u0,
        created-at: current-block
      }
    )
    (var-set next-course-id (+ course-id u1))
    (ok course-id)
  )
)

(define-public (enroll-in-course (course-id uint))
  (let
    (
      (course (unwrap! (map-get? courses course-id) ERR_COURSE_NOT_FOUND))
      (enrollment-key {course-id: course-id, student: tx-sender})
      (current-block stacks-block-height)
      (course-price (get price course))
      (platform-fee (/ (* course-price PLATFORM_FEE_PERCENT) u100))
      (instructor-payment (- course-price platform-fee))
    )
    (asserts! (get is-active course) ERR_INVALID_COURSE)
    (asserts! (is-none (map-get? enrollments enrollment-key)) ERR_ALREADY_ENROLLED)
    (asserts! (>= (stx-get-balance tx-sender) course-price) ERR_INSUFFICIENT_PAYMENT)
    
    (try! (stx-transfer? instructor-payment tx-sender (get instructor course)))
    (var-set platform-earnings (+ (var-get platform-earnings) platform-fee))
    
    (map-set enrollments enrollment-key
      {
        enrolled-at: current-block,
        completed-at: none,
        certificate-id: none,
        payment-amount: course-price
      }
    )
    
    (map-set courses course-id
      (merge course {total-enrolled: (+ (get total-enrolled course) u1)})
    )
    
    (map-set instructor-earnings (get instructor course)
      (+ (default-to u0 (map-get? instructor-earnings (get instructor course))) instructor-payment)
    )
    
    (ok true)
  )
)

(define-public (complete-course (course-id uint))
  (let
    (
      (enrollment-key {course-id: course-id, student: tx-sender})
      (enrollment (unwrap! (map-get? enrollments enrollment-key) ERR_NOT_ENROLLED))
      (course (unwrap! (map-get? courses course-id) ERR_COURSE_NOT_FOUND))
      (current-block stacks-block-height)
      (required-blocks (+ (get enrolled-at enrollment) (get duration-blocks course)))
    )
    (asserts! (>= current-block required-blocks) ERR_COURSE_NOT_COMPLETED)
    (asserts! (is-none (get completed-at enrollment)) ERR_COURSE_NOT_COMPLETED)
    
    (map-set enrollments enrollment-key
      (merge enrollment {completed-at: (some current-block)})
    )
    (ok true)
  )
)

(define-public (issue-certificate (course-id uint))
  (let
    (
      (enrollment-key {course-id: course-id, student: tx-sender})
      (enrollment (unwrap! (map-get? enrollments enrollment-key) ERR_NOT_ENROLLED))
      (course (unwrap! (map-get? courses course-id) ERR_COURSE_NOT_FOUND))
      (certificate-id (var-get next-certificate-id))
      (current-block stacks-block-height)
    )
    (asserts! (is-some (get completed-at enrollment)) ERR_COURSE_NOT_COMPLETED)
    (asserts! (is-none (get certificate-id enrollment)) ERR_CERTIFICATE_ALREADY_ISSUED)
    
    (try! (nft-mint? course-certificate certificate-id tx-sender))
    
    (map-set certificates certificate-id
      {
        course-id: course-id,
        student: tx-sender,
        instructor: (get instructor course),
        issued-at: current-block,
        course-title: (get title course)
      }
    )
    
    (map-set enrollments enrollment-key
      (merge enrollment {certificate-id: (some certificate-id)})
    )
    
    (var-set next-certificate-id (+ certificate-id u1))
    (ok certificate-id)
  )
)

(define-public (add-review (course-id uint) (rating uint) (review (string-ascii 200)))
  (let
    (
      (enrollment-key {course-id: course-id, student: tx-sender})
      (enrollment (unwrap! (map-get? enrollments enrollment-key) ERR_NOT_ENROLLED))
      (review-key {course-id: course-id, student: tx-sender})
      (current-block stacks-block-height)
    )
    (asserts! (is-some (get completed-at enrollment)) ERR_COURSE_NOT_COMPLETED)
    (asserts! (<= rating u5) ERR_INVALID_COURSE)
    (asserts! (>= rating u1) ERR_INVALID_COURSE)
    
    (map-set course-reviews review-key
      {
        rating: rating,
        review: review,
        created-at: current-block
      }
    )
    (ok true)
  )
)

(define-public (deactivate-course (course-id uint))
  (let
    (
      (course (unwrap! (map-get? courses course-id) ERR_COURSE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get instructor course)) ERR_NOT_AUTHORIZED)
    
    (map-set courses course-id
      (merge course {is-active: false})
    )
    (ok true)
  )
)

(define-public (withdraw-platform-earnings)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (let ((earnings (var-get platform-earnings)))
      (var-set platform-earnings u0)
      (try! (as-contract (stx-transfer? earnings tx-sender CONTRACT_OWNER)))
      (ok earnings)
    )
  )
)

(define-read-only (get-course (course-id uint))
  (map-get? courses course-id)
)

(define-read-only (get-enrollment (course-id uint) (student principal))
  (map-get? enrollments {course-id: course-id, student: student})
)

(define-read-only (get-certificate (certificate-id uint))
  (map-get? certificates certificate-id)
)

(define-read-only (get-instructor-earnings (instructor principal))
  (default-to u0 (map-get? instructor-earnings instructor))
)

(define-read-only (get-platform-earnings)
  (var-get platform-earnings)
)

(define-read-only (get-course-review (course-id uint) (student principal))
  (map-get? course-reviews {course-id: course-id, student: student})
)

(define-read-only (is-enrolled (course-id uint) (student principal))
  (is-some (map-get? enrollments {course-id: course-id, student: student}))
)

(define-read-only (has-completed-course (course-id uint) (student principal))
  (match (map-get? enrollments {course-id: course-id, student: student})
    enrollment (is-some (get completed-at enrollment))
    false
  )
)

(define-read-only (get-certificate-owner (certificate-id uint))
  (nft-get-owner? course-certificate certificate-id)
)

(define-read-only (get-next-course-id)
  (var-get next-course-id)
)

(define-read-only (get-next-certificate-id)
  (var-get next-certificate-id)
)