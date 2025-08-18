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
(define-constant ERR_ASSESSMENT_NOT_FOUND (err u108))
(define-constant ERR_ASSESSMENT_ALREADY_TAKEN (err u109))
(define-constant ERR_INVALID_SCORE (err u110))
(define-constant ERR_SKILL_NOT_FOUND (err u111))
(define-constant ERR_ASSESSMENT_NOT_COMPLETED (err u112))
(define-constant ERR_INSUFFICIENT_SKILL_LEVEL (err u113))
(define-constant ERR_INVALID_ASSESSMENT (err u114))
(define-constant ERR_PREREQUISITE_NOT_MET (err u115))
(define-constant ERR_TUTOR_NOT_FOUND (err u116))
(define-constant ERR_SESSION_NOT_FOUND (err u117))
(define-constant ERR_INVALID_SESSION_TIME (err u118))
(define-constant ERR_SESSION_ALREADY_BOOKED (err u119))
(define-constant ERR_NOT_SESSION_PARTICIPANT (err u120))
(define-constant ERR_SESSION_NOT_COMPLETED (err u121))
(define-constant ERR_TUTOR_NOT_AVAILABLE (err u122))
(define-constant ERR_INVALID_TUTORING_RATE (err u123))
(define-constant ERR_SESSION_ALREADY_RATED (err u124))
(define-constant PLATFORM_FEE_PERCENT u5)
(define-constant TUTORING_PLATFORM_FEE_PERCENT u10)

(define-data-var next-course-id uint u1)
(define-data-var next-certificate-id uint u1)
(define-data-var next-assessment-id uint u1)
(define-data-var next-skill-id uint u1)
(define-data-var next-session-id uint u1)
(define-data-var platform-earnings uint u0)
(define-data-var tutoring-platform-earnings uint u0)

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

(define-map skills
  uint
  {
    name: (string-ascii 50),
    category: (string-ascii 30),
    description: (string-ascii 200),
    creator: principal,
    created-at: uint,
    is-active: bool
  }
)

(define-map skill-assessments
  uint
  {
    skill-id: uint,
    title: (string-ascii 100),
    description: (string-ascii 300),
    creator: principal,
    max-score: uint,
    passing-score: uint,
    time-limit-blocks: uint,
    is-active: bool,
    total-attempts: uint,
    created-at: uint
  }
)

(define-map assessment-attempts
  {assessment-id: uint, student: principal}
  {
    started-at: uint,
    completed-at: (optional uint),
    score: (optional uint),
    passed: bool,
    time-taken-blocks: (optional uint)
  }
)

(define-map student-skills
  {student: principal, skill-id: uint}
  {
    current-level: uint,
    total-assessments: uint,
    last-assessment-at: uint,
    best-score: uint,
    skill-points: uint
  }
)

(define-map course-skill-requirements
  {course-id: uint, skill-id: uint}
  {
    required-level: uint,
    is-prerequisite: bool
  }
)

(define-map skill-competency-levels
  {skill-id: uint, level: uint}
  {
    level-name: (string-ascii 20),
    min-score: uint,
    skill-points-awarded: uint
  }
)

(define-map tutor-profiles
  principal
  {
    bio: (string-ascii 300),
    specialties: (string-ascii 200),
    hourly-rate: uint,
    is-available: bool,
    total-sessions: uint,
    total-earnings: uint,
    average-rating: uint,
    total-ratings: uint,
    created-at: uint
  }
)

(define-map tutoring-sessions
  uint
  {
    tutor: principal,
    student: (optional principal),
    skill-id: uint,
    duration-blocks: uint,
    total-cost: uint,
    scheduled-at: uint,
    started-at: (optional uint),
    completed-at: (optional uint),
    is-cancelled: bool,
    payment-released: bool
  }
)

(define-map session-ratings
  {session-id: uint, rater: principal}
  {
    rating: uint,
    feedback: (string-ascii 200),
    created-at: uint
  }
)

(define-map tutor-availability
  {tutor: principal, time-slot: uint}
  {
    is-available: bool,
    max-sessions: uint,
    current-bookings: uint
  }
)

(define-map student-session-history
  {student: principal, session-id: uint}
  {
    session-cost: uint,
    completed: bool,
    rating-given: bool
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

(define-public (create-skill (name (string-ascii 50)) (category (string-ascii 30)) (description (string-ascii 200)))
  (let
    (
      (skill-id (var-get next-skill-id))
      (current-block stacks-block-height)
    )
    (map-set skills skill-id
      {
        name: name,
        category: category,
        description: description,
        creator: tx-sender,
        created-at: current-block,
        is-active: true
      }
    )
    (var-set next-skill-id (+ skill-id u1))
    (ok skill-id)
  )
)

(define-public (create-assessment (skill-id uint) (title (string-ascii 100)) (description (string-ascii 300)) (max-score uint) (passing-score uint) (time-limit-blocks uint))
  (let
    (
      (assessment-id (var-get next-assessment-id))
      (current-block stacks-block-height)
      (skill (unwrap! (map-get? skills skill-id) ERR_SKILL_NOT_FOUND))
    )
    (asserts! (get is-active skill) ERR_SKILL_NOT_FOUND)
    (asserts! (> max-score u0) ERR_INVALID_ASSESSMENT)
    (asserts! (<= passing-score max-score) ERR_INVALID_ASSESSMENT)
    (asserts! (> time-limit-blocks u0) ERR_INVALID_ASSESSMENT)
    
    (map-set skill-assessments assessment-id
      {
        skill-id: skill-id,
        title: title,
        description: description,
        creator: tx-sender,
        max-score: max-score,
        passing-score: passing-score,
        time-limit-blocks: time-limit-blocks,
        is-active: true,
        total-attempts: u0,
        created-at: current-block
      }
    )
    (var-set next-assessment-id (+ assessment-id u1))
    (ok assessment-id)
  )
)

(define-public (start-assessment (assessment-id uint))
  (let
    (
      (assessment (unwrap! (map-get? skill-assessments assessment-id) ERR_ASSESSMENT_NOT_FOUND))
      (attempt-key {assessment-id: assessment-id, student: tx-sender})
      (current-block stacks-block-height)
    )
    (asserts! (get is-active assessment) ERR_INVALID_ASSESSMENT)
    (asserts! (is-none (map-get? assessment-attempts attempt-key)) ERR_ASSESSMENT_ALREADY_TAKEN)
    
    (map-set assessment-attempts attempt-key
      {
        started-at: current-block,
        completed-at: none,
        score: none,
        passed: false,
        time-taken-blocks: none
      }
    )
    
    (map-set skill-assessments assessment-id
      (merge assessment {total-attempts: (+ (get total-attempts assessment) u1)})
    )
    (ok true)
  )
)

(define-public (complete-assessment (assessment-id uint) (score uint))
  (let
    (
      (assessment (unwrap! (map-get? skill-assessments assessment-id) ERR_ASSESSMENT_NOT_FOUND))
      (attempt-key {assessment-id: assessment-id, student: tx-sender})
      (attempt (unwrap! (map-get? assessment-attempts attempt-key) ERR_ASSESSMENT_NOT_COMPLETED))
      (current-block stacks-block-height)
      (time-taken (- current-block (get started-at attempt)))
      (max-time (get time-limit-blocks assessment))
      (passed (>= score (get passing-score assessment)))
      (skill-id (get skill-id assessment))
    )
    (asserts! (is-none (get completed-at attempt)) ERR_ASSESSMENT_ALREADY_TAKEN)
    (asserts! (<= score (get max-score assessment)) ERR_INVALID_SCORE)
    (asserts! (<= time-taken max-time) ERR_INVALID_ASSESSMENT)
    
    (map-set assessment-attempts attempt-key
      (merge attempt {
        completed-at: (some current-block),
        score: (some score),
        passed: passed,
        time-taken-blocks: (some time-taken)
      })
    )
    
    (if passed
      (begin
        (unwrap! (update-student-skill-level skill-id score) ERR_INVALID_ASSESSMENT)
        (ok true)
      )
      (ok false)
    )
  )
)

(define-public (set-skill-competency-level (skill-id uint) (level uint) (level-name (string-ascii 20)) (min-score uint) (skill-points-awarded uint))
  (let
    (
      (skill (unwrap! (map-get? skills skill-id) ERR_SKILL_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator skill)) ERR_NOT_AUTHORIZED)
    (asserts! (> level u0) ERR_INVALID_ASSESSMENT)
    (asserts! (> skill-points-awarded u0) ERR_INVALID_ASSESSMENT)
    
    (map-set skill-competency-levels {skill-id: skill-id, level: level}
      {
        level-name: level-name,
        min-score: min-score,
        skill-points-awarded: skill-points-awarded
      }
    )
    (ok true)
  )
)

(define-public (add-course-skill-requirement (course-id uint) (skill-id uint) (required-level uint) (is-prerequisite bool))
  (let
    (
      (course (unwrap! (map-get? courses course-id) ERR_COURSE_NOT_FOUND))
      (skill (unwrap! (map-get? skills skill-id) ERR_SKILL_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get instructor course)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active skill) ERR_SKILL_NOT_FOUND)
    (asserts! (> required-level u0) ERR_INVALID_ASSESSMENT)
    
    (map-set course-skill-requirements {course-id: course-id, skill-id: skill-id}
      {
        required-level: required-level,
        is-prerequisite: is-prerequisite
      }
    )
    (ok true)
  )
)

(define-public (check-skill-prerequisites (course-id uint))
  (let
    (
      (course (unwrap! (map-get? courses course-id) ERR_COURSE_NOT_FOUND))
    )
    (ok (check-all-prerequisites course-id tx-sender))
  )
)

(define-private (update-student-skill-level (skill-id uint) (new-score uint))
  (let
    (
      (student-skill-key {student: tx-sender, skill-id: skill-id})
      (current-skill (default-to 
        {current-level: u0, total-assessments: u0, last-assessment-at: u0, best-score: u0, skill-points: u0}
        (map-get? student-skills student-skill-key)
      ))
      (current-block stacks-block-height)
      (new-level (calculate-skill-level skill-id new-score))
      (best-score (if (> new-score (get best-score current-skill)) new-score (get best-score current-skill)))
      (skill-points (calculate-skill-points skill-id new-level))
    )
    (map-set student-skills student-skill-key
      {
        current-level: new-level,
        total-assessments: (+ (get total-assessments current-skill) u1),
        last-assessment-at: current-block,
        best-score: best-score,
        skill-points: skill-points
      }
    )
    (ok true)
  )
)

(define-private (calculate-skill-level (skill-id uint) (score uint))
  (let
    (
      (level-1 (map-get? skill-competency-levels {skill-id: skill-id, level: u1}))
      (level-2 (map-get? skill-competency-levels {skill-id: skill-id, level: u2}))
      (level-3 (map-get? skill-competency-levels {skill-id: skill-id, level: u3}))
      (level-4 (map-get? skill-competency-levels {skill-id: skill-id, level: u4}))
      (level-5 (map-get? skill-competency-levels {skill-id: skill-id, level: u5}))
    )
    (if (and (is-some level-5) (>= score (get min-score (unwrap-panic level-5))))
      u5
      (if (and (is-some level-4) (>= score (get min-score (unwrap-panic level-4))))
        u4
        (if (and (is-some level-3) (>= score (get min-score (unwrap-panic level-3))))
          u3
          (if (and (is-some level-2) (>= score (get min-score (unwrap-panic level-2))))
            u2
            (if (and (is-some level-1) (>= score (get min-score (unwrap-panic level-1))))
              u1
              u0
            )
          )
        )
      )
    )
  )
)

(define-private (calculate-skill-points (skill-id uint) (level uint))
  (match (map-get? skill-competency-levels {skill-id: skill-id, level: level})
    competency-level (get skill-points-awarded competency-level)
    u0
  )
)

(define-private (check-all-prerequisites (course-id uint) (student principal))
  (let
    (
      (requirement-1 (map-get? course-skill-requirements {course-id: course-id, skill-id: u1}))
      (requirement-2 (map-get? course-skill-requirements {course-id: course-id, skill-id: u2}))
      (requirement-3 (map-get? course-skill-requirements {course-id: course-id, skill-id: u3}))
    )
    (and
      (check-single-prerequisite student requirement-1 u1)
      (check-single-prerequisite student requirement-2 u2)
      (check-single-prerequisite student requirement-3 u3)
    )
  )
)

(define-private (check-single-prerequisite (student principal) (requirement (optional {required-level: uint, is-prerequisite: bool})) (skill-id uint))
  (match requirement
    req
      (if (get is-prerequisite req)
        (let
          (
            (student-skill (map-get? student-skills {student: student, skill-id: skill-id}))
          )
          (match student-skill
            skill (>= (get current-level skill) (get required-level req))
            false
          )
        )
        true
      )
    true
  )
)

(define-public (become-tutor (bio (string-ascii 300)) (specialties (string-ascii 200)) (hourly-rate uint))
  (let
    (
      (current-block stacks-block-height)
    )
    (asserts! (> hourly-rate u0) ERR_INVALID_TUTORING_RATE)
    (asserts! (<= hourly-rate u10000) ERR_INVALID_TUTORING_RATE)
    
    (map-set tutor-profiles tx-sender
      {
        bio: bio,
        specialties: specialties,
        hourly-rate: hourly-rate,
        is-available: true,
        total-sessions: u0,
        total-earnings: u0,
        average-rating: u0,
        total-ratings: u0,
        created-at: current-block
      }
    )
    (ok true)
  )
)

(define-public (update-tutor-availability (is-available bool))
  (let
    (
      (tutor-profile (unwrap! (map-get? tutor-profiles tx-sender) ERR_TUTOR_NOT_FOUND))
    )
    (map-set tutor-profiles tx-sender
      (merge tutor-profile {is-available: is-available})
    )
    (ok true)
  )
)

(define-public (create-tutoring-session (skill-id uint) (duration-blocks uint) (scheduled-at uint))
  (let
    (
      (session-id (var-get next-session-id))
      (tutor-profile (unwrap! (map-get? tutor-profiles tx-sender) ERR_TUTOR_NOT_FOUND))
      (skill (unwrap! (map-get? skills skill-id) ERR_SKILL_NOT_FOUND))
      (total-cost (* (get hourly-rate tutor-profile) duration-blocks))
      (current-block stacks-block-height)
    )
    (asserts! (get is-available tutor-profile) ERR_TUTOR_NOT_AVAILABLE)
    (asserts! (get is-active skill) ERR_SKILL_NOT_FOUND)
    (asserts! (> duration-blocks u0) ERR_INVALID_SESSION_TIME)
    (asserts! (> scheduled-at current-block) ERR_INVALID_SESSION_TIME)
    
    (map-set tutoring-sessions session-id
      {
        tutor: tx-sender,
        student: none,
        skill-id: skill-id,
        duration-blocks: duration-blocks,
        total-cost: total-cost,
        scheduled-at: scheduled-at,
        started-at: none,
        completed-at: none,
        is-cancelled: false,
        payment-released: false
      }
    )
    (var-set next-session-id (+ session-id u1))
    (ok session-id)
  )
)

(define-public (book-tutoring-session (session-id uint))
  (let
    (
      (session (unwrap! (map-get? tutoring-sessions session-id) ERR_SESSION_NOT_FOUND))
      (tutor-profile (unwrap! (map-get? tutor-profiles (get tutor session)) ERR_TUTOR_NOT_FOUND))
      (session-cost (get total-cost session))
      (platform-fee (/ (* session-cost TUTORING_PLATFORM_FEE_PERCENT) u100))
      (tutor-payment (- session-cost platform-fee))
      (current-block stacks-block-height)
    )
    (asserts! (is-none (get student session)) ERR_SESSION_ALREADY_BOOKED)
    (asserts! (not (get is-cancelled session)) ERR_SESSION_NOT_FOUND)
    (asserts! (> (get scheduled-at session) current-block) ERR_INVALID_SESSION_TIME)
    (asserts! (>= (stx-get-balance tx-sender) session-cost) ERR_INSUFFICIENT_PAYMENT)
    
    (try! (stx-transfer? session-cost tx-sender (as-contract tx-sender)))
    
    (map-set tutoring-sessions session-id
      (merge session {student: (some tx-sender)})
    )
    
    (map-set student-session-history {student: tx-sender, session-id: session-id}
      {
        session-cost: session-cost,
        completed: false,
        rating-given: false
      }
    )
    (ok true)
  )
)

(define-public (start-tutoring-session (session-id uint))
  (let
    (
      (session (unwrap! (map-get? tutoring-sessions session-id) ERR_SESSION_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (or (is-eq tx-sender (get tutor session)) 
                  (is-eq (some tx-sender) (get student session))) ERR_NOT_SESSION_PARTICIPANT)
    (asserts! (is-some (get student session)) ERR_SESSION_NOT_FOUND)
    (asserts! (is-none (get started-at session)) ERR_SESSION_ALREADY_BOOKED)
    (asserts! (not (get is-cancelled session)) ERR_SESSION_NOT_FOUND)
    
    (map-set tutoring-sessions session-id
      (merge session {started-at: (some current-block)})
    )
    (ok true)
  )
)

(define-public (complete-tutoring-session (session-id uint))
  (let
    (
      (session (unwrap! (map-get? tutoring-sessions session-id) ERR_SESSION_NOT_FOUND))
      (tutor-profile (unwrap! (map-get? tutor-profiles (get tutor session)) ERR_TUTOR_NOT_FOUND))
      (session-cost (get total-cost session))
      (platform-fee (/ (* session-cost TUTORING_PLATFORM_FEE_PERCENT) u100))
      (tutor-payment (- session-cost platform-fee))
      (current-block stacks-block-height)
      (student (unwrap! (get student session) ERR_SESSION_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get tutor session)) 
                  (is-eq tx-sender student)) ERR_NOT_SESSION_PARTICIPANT)
    (asserts! (is-some (get started-at session)) ERR_SESSION_NOT_COMPLETED)
    (asserts! (is-none (get completed-at session)) ERR_SESSION_ALREADY_BOOKED)
    (asserts! (not (get payment-released session)) ERR_SESSION_ALREADY_BOOKED)
    
    (try! (as-contract (stx-transfer? tutor-payment tx-sender (get tutor session))))
    (var-set tutoring-platform-earnings (+ (var-get tutoring-platform-earnings) platform-fee))
    
    (map-set tutoring-sessions session-id
      (merge session {
        completed-at: (some current-block),
        payment-released: true
      })
    )
    
    (map-set tutor-profiles (get tutor session)
      (merge tutor-profile {
        total-sessions: (+ (get total-sessions tutor-profile) u1),
        total-earnings: (+ (get total-earnings tutor-profile) tutor-payment)
      })
    )
    
    (map-set student-session-history {student: student, session-id: session-id}
      (merge 
        (unwrap! (map-get? student-session-history {student: student, session-id: session-id}) ERR_SESSION_NOT_FOUND)
        {completed: true}
      )
    )
    (ok true)
  )
)

(define-public (rate-tutor (session-id uint) (rating uint) (feedback (string-ascii 200)))
  (let
    (
      (session (unwrap! (map-get? tutoring-sessions session-id) ERR_SESSION_NOT_FOUND))
      (tutor-profile (unwrap! (map-get? tutor-profiles (get tutor session)) ERR_TUTOR_NOT_FOUND))
      (student (unwrap! (get student session) ERR_SESSION_NOT_FOUND))
      (session-history (unwrap! (map-get? student-session-history {student: tx-sender, session-id: session-id}) ERR_SESSION_NOT_FOUND))
      (rating-key {session-id: session-id, rater: tx-sender})
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender student) ERR_NOT_SESSION_PARTICIPANT)
    (asserts! (get completed session-history) ERR_SESSION_NOT_COMPLETED)
    (asserts! (not (get rating-given session-history)) ERR_SESSION_ALREADY_RATED)
    (asserts! (>= rating u1) ERR_INVALID_ASSESSMENT)
    (asserts! (<= rating u5) ERR_INVALID_ASSESSMENT)
    (asserts! (is-some (get completed-at session)) ERR_SESSION_NOT_COMPLETED)
    (asserts! (is-none (map-get? session-ratings rating-key)) ERR_SESSION_ALREADY_RATED)
    
    (map-set session-ratings rating-key
      {
        rating: rating,
        feedback: feedback,
        created-at: current-block
      }
    )
    
    (let
      (
        (total-ratings (+ (get total-ratings tutor-profile) u1))
        (current-total (+ (* (get average-rating tutor-profile) (get total-ratings tutor-profile)) rating))
        (new-average (/ current-total total-ratings))
      )
      (map-set tutor-profiles (get tutor session)
        (merge tutor-profile {
          average-rating: new-average,
          total-ratings: total-ratings
        })
      )
    )
    
    (map-set student-session-history {student: tx-sender, session-id: session-id}
      (merge session-history {rating-given: true})
    )
    (ok true)
  )
)

(define-public (cancel-tutoring-session (session-id uint))
  (let
    (
      (session (unwrap! (map-get? tutoring-sessions session-id) ERR_SESSION_NOT_FOUND))
      (current-block stacks-block-height)
      (student (get student session))
    )
    (asserts! (or (is-eq tx-sender (get tutor session))
                  (and (is-some student) (is-eq tx-sender (unwrap-panic student)))) ERR_NOT_SESSION_PARTICIPANT)
    (asserts! (is-none (get started-at session)) ERR_SESSION_ALREADY_BOOKED)
    (asserts! (not (get is-cancelled session)) ERR_SESSION_NOT_FOUND)
    
    (if (is-some student)
      (begin
        (try! (as-contract (stx-transfer? (get total-cost session) tx-sender (unwrap-panic student))))
        true
      )
      true
    )
    
    (map-set tutoring-sessions session-id
      (merge session {is-cancelled: true})
    )
    (ok true)
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

(define-read-only (get-skill (skill-id uint))
  (map-get? skills skill-id)
)

(define-read-only (get-assessment (assessment-id uint))
  (map-get? skill-assessments assessment-id)
)

(define-read-only (get-assessment-attempt (assessment-id uint) (student principal))
  (map-get? assessment-attempts {assessment-id: assessment-id, student: student})
)

(define-read-only (get-student-skill (student principal) (skill-id uint))
  (map-get? student-skills {student: student, skill-id: skill-id})
)

(define-read-only (get-course-skill-requirement (course-id uint) (skill-id uint))
  (map-get? course-skill-requirements {course-id: course-id, skill-id: skill-id})
)

(define-read-only (get-skill-competency-level (skill-id uint) (level uint))
  (map-get? skill-competency-levels {skill-id: skill-id, level: level})
)

(define-read-only (get-student-total-skill-points (student principal))
  (let
    (
      (skill-1-data (map-get? student-skills {student: student, skill-id: u1}))
      (skill-2-data (map-get? student-skills {student: student, skill-id: u2}))
      (skill-3-data (map-get? student-skills {student: student, skill-id: u3}))
      (skill-4-data (map-get? student-skills {student: student, skill-id: u4}))
      (skill-5-data (map-get? student-skills {student: student, skill-id: u5}))
      (skill-1 (if (is-some skill-1-data) (get skill-points (unwrap-panic skill-1-data)) u0))
      (skill-2 (if (is-some skill-2-data) (get skill-points (unwrap-panic skill-2-data)) u0))
      (skill-3 (if (is-some skill-3-data) (get skill-points (unwrap-panic skill-3-data)) u0))
      (skill-4 (if (is-some skill-4-data) (get skill-points (unwrap-panic skill-4-data)) u0))
      (skill-5 (if (is-some skill-5-data) (get skill-points (unwrap-panic skill-5-data)) u0))
    )
    (+ skill-1 skill-2 skill-3 skill-4 skill-5)
  )
)

(define-read-only (has-passed-assessment (assessment-id uint) (student principal))
  (match (map-get? assessment-attempts {assessment-id: assessment-id, student: student})
    attempt (get passed attempt)
    false
  )
)

(define-read-only (get-student-skill-level (student principal) (skill-id uint))
  (match (map-get? student-skills {student: student, skill-id: skill-id})
    skill (get current-level skill)
    u0
  )
)

(define-read-only (meets-course-prerequisites (course-id uint) (student principal))
  (check-all-prerequisites course-id student)
)

(define-read-only (get-next-skill-id)
  (var-get next-skill-id)
)

(define-read-only (get-next-assessment-id)
  (var-get next-assessment-id)
)

(define-read-only (get-tutor-profile (tutor principal))
  (map-get? tutor-profiles tutor)
)

(define-read-only (get-tutoring-session (session-id uint))
  (map-get? tutoring-sessions session-id)
)

(define-read-only (get-session-rating (session-id uint) (rater principal))
  (map-get? session-ratings {session-id: session-id, rater: rater})
)

(define-read-only (get-student-session-history (student principal) (session-id uint))
  (map-get? student-session-history {student: student, session-id: session-id})
)

(define-read-only (is-tutor (user principal))
  (is-some (map-get? tutor-profiles user))
)

(define-read-only (get-tutor-availability (tutor principal) (time-slot uint))
  (map-get? tutor-availability {tutor: tutor, time-slot: time-slot})
)

(define-read-only (get-tutoring-platform-earnings)
  (var-get tutoring-platform-earnings)
)

(define-read-only (get-next-session-id)
  (var-get next-session-id)
)

(define-read-only (is-session-participant (session-id uint) (user principal))
  (match (map-get? tutoring-sessions session-id)
    session (or (is-eq user (get tutor session))
                (is-eq (some user) (get student session)))
    false
  )
)

(define-read-only (get-available-tutors-count (skill-id uint))
  u0
)

(define-read-only (calculate-session-cost (tutor principal) (duration-blocks uint))
  (match (map-get? tutor-profiles tutor)
    profile (* (get hourly-rate profile) duration-blocks)
    u0
  )
)








