;; Social Learning Groups Contract for Learnbit-STX
;; Collaborative learning platform for study groups, shared goals, and peer-based rewards

;; Error constants
(define-constant err-unauthorized (err u300))
(define-constant err-group-not-found (err u301))
(define-constant err-already-member (err u302))
(define-constant err-not-member (err u303))
(define-constant err-group-full (err u304))
(define-constant err-invalid-group-size (err u305))
(define-constant err-goal-not-found (err u306))
(define-constant err-goal-already-exists (err u307))
(define-constant err-goal-not-active (err u308))
(define-constant err-insufficient-progress (err u309))
(define-constant err-achievement-not-found (err u310))
(define-constant err-already-claimed (err u311))
(define-constant err-challenge-not-found (err u312))
(define-constant err-invalid-deadline (err u313))
(define-constant err-peer-review-exists (err u314))
(define-constant err-cannot-review-self (err u315))

;; Contract constants
(define-constant group-creation-fee u100000) ;; 0.1 STX
(define-constant max-group-size u10)
(define-constant min-group-size u2)
(define-constant max-active-goals u5)
(define-constant peer-review-reward u50000) ;; 0.05 STX

;; Data variables
(define-data-var next-group-id uint u1)
(define-data-var next-goal-id uint u1)
(define-data-var next-achievement-id uint u1)
(define-data-var next-challenge-id uint u1)
(define-data-var platform-rewards-pool uint u0)

;; Learning groups data structure
(define-map learning-groups
    uint
    {
        name: (string-ascii 64),
        description: (string-ascii 200),
        creator: principal,
        max-members: uint,
        current-members: uint,
        is-public: bool,
        is-active: bool,
        created-at: uint,
        total-goals-completed: uint,
        group-experience-points: uint,
        group-level: uint
    }
)

;; Group membership tracking
(define-map group-members
    {group-id: uint, member: principal}
    {
        joined-at: uint,
        role: (string-ascii 20), ;; "member", "moderator", "leader"
        contribution-score: uint,
        peer-reviews-given: uint,
        peer-reviews-received: uint,
        goals-contributed: uint,
        is-active: bool
    }
)

;; Group goals for collaborative learning
(define-map group-goals
    uint
    {
        group-id: uint,
        title: (string-ascii 100),
        description: (string-ascii 300),
        goal-type: (string-ascii 30), ;; "course-completion", "skill-mastery", "study-hours", "peer-review"
        target-value: uint,
        current-progress: uint,
        deadline: uint,
        reward-pool: uint,
        created-by: principal,
        created-at: uint,
        is-active: bool,
        is-completed: bool,
        completion-date: (optional uint)
    }
)

;; Individual contributions to group goals
(define-map goal-contributions
    {goal-id: uint, contributor: principal}
    {
        contribution-value: uint,
        last-updated: uint,
        milestones-achieved: uint,
        has-claimed-reward: bool
    }
)

;; Peer review system for group learning
(define-map peer-reviews
    {reviewer: principal, reviewee: principal, group-id: uint}
    {
        review-type: (string-ascii 30), ;; "knowledge-sharing", "collaboration", "leadership"
        rating: uint, ;; 1-5 stars
        feedback: (string-ascii 300),
        created-at: uint,
        is-verified: bool,
        reward-claimed: bool
    }
)

;; Group leaderboard tracking
(define-map group-leaderboard
    uint ;; group-id
    {
        rank: uint,
        total-score: uint,
        challenges-won: uint,
        goals-completed: uint,
        members-count: uint,
        average-contribution: uint
    }
)

;; Member activity and engagement tracking
(define-map member-activity
    {member: principal, group-id: uint}
    {
        last-active: uint,
        messages-sent: uint,
        goals-created: uint,
        sessions-attended: uint,
        peer-interactions: uint,
        weekly-engagement-score: uint
    }
)

;; Create a new learning group
(define-public (create-learning-group (name (string-ascii 64)) (description (string-ascii 200)) (max-members uint) (is-public bool))
    (let (
        (group-id (var-get next-group-id))
        (current-block stacks-block-height)
    )
        (asserts! (and (>= max-members min-group-size) (<= max-members max-group-size)) err-invalid-group-size)
        (asserts! (>= (stx-get-balance tx-sender) group-creation-fee) err-unauthorized)
        
        ;; Charge group creation fee
        (try! (stx-transfer? group-creation-fee tx-sender (as-contract tx-sender)))
        (var-set platform-rewards-pool (+ (var-get platform-rewards-pool) group-creation-fee))
        
        ;; Create the group
        (map-set learning-groups group-id
            {
                name: name,
                description: description,
                creator: tx-sender,
                max-members: max-members,
                current-members: u1,
                is-public: is-public,
                is-active: true,
                created-at: current-block,
                total-goals-completed: u0,
                group-experience-points: u0,
                group-level: u1
            }
        )
        
        ;; Add creator as group leader
        (map-set group-members {group-id: group-id, member: tx-sender}
            {
                joined-at: current-block,
                role: "leader",
                contribution-score: u0,
                peer-reviews-given: u0,
                peer-reviews-received: u0,
                goals-contributed: u0,
                is-active: true
            }
        )
        
        ;; Initialize group leaderboard entry
        (map-set group-leaderboard group-id
            {
                rank: u0,
                total-score: u0,
                challenges-won: u0,
                goals-completed: u0,
                members-count: u1,
                average-contribution: u0
            }
        )
        
        (var-set next-group-id (+ group-id u1))
        (ok group-id)
    )
)

;; Join an existing learning group
(define-public (join-learning-group (group-id uint))
    (let (
        (group-data (unwrap! (map-get? learning-groups group-id) err-group-not-found))
        (current-block stacks-block-height)
        (member-key {group-id: group-id, member: tx-sender})
    )
        (asserts! (get is-active group-data) err-group-not-found)
        (asserts! (< (get current-members group-data) (get max-members group-data)) err-group-full)
        (asserts! (is-none (map-get? group-members member-key)) err-already-member)
        
        ;; Add member to group
        (map-set group-members member-key
            {
                joined-at: current-block,
                role: "member",
                contribution-score: u0,
                peer-reviews-given: u0,
                peer-reviews-received: u0,
                goals-contributed: u0,
                is-active: true
            }
        )
        
        ;; Update group member count
        (map-set learning-groups group-id
            (merge group-data {
                current-members: (+ (get current-members group-data) u1)
            })
        )
        
        ;; Initialize member activity
        (map-set member-activity {member: tx-sender, group-id: group-id}
            {
                last-active: current-block,
                messages-sent: u0,
                goals-created: u0,
                sessions-attended: u0,
                peer-interactions: u0,
                weekly-engagement-score: u0
            }
        )
        
        (ok true)
    )
)

;; Create a collaborative learning goal for the group
(define-public (create-group-goal (group-id uint) (title (string-ascii 100)) (description (string-ascii 300)) 
                                  (goal-type (string-ascii 30)) (target-value uint) (deadline uint) (reward-pool uint))
    (let (
        (goal-id (var-get next-goal-id))
        (group-data (unwrap! (map-get? learning-groups group-id) err-group-not-found))
        (member-data (unwrap! (map-get? group-members {group-id: group-id, member: tx-sender}) err-not-member))
        (current-block stacks-block-height)
    )
        (asserts! (get is-active group-data) err-group-not-found)
        (asserts! (get is-active member-data) err-not-member)
        (asserts! (> deadline current-block) err-invalid-deadline)
        (asserts! (> target-value u0) err-goal-not-found)
        
        ;; Create the group goal
        (map-set group-goals goal-id
            {
                group-id: group-id,
                title: title,
                description: description,
                goal-type: goal-type,
                target-value: target-value,
                current-progress: u0,
                deadline: deadline,
                reward-pool: reward-pool,
                created-by: tx-sender,
                created-at: current-block,
                is-active: true,
                is-completed: false,
                completion-date: none
            }
        )
        
        ;; Initialize contributor's contribution
        (map-set goal-contributions {goal-id: goal-id, contributor: tx-sender}
            {
                contribution-value: u0,
                last-updated: current-block,
                milestones-achieved: u0,
                has-claimed-reward: false
            }
        )
        
        ;; Update member's goal contribution count
        (map-set group-members {group-id: group-id, member: tx-sender}
            (merge member-data {
                goals-contributed: (+ (get goals-contributed member-data) u1)
            })
        )
        
        (var-set next-goal-id (+ goal-id u1))
        (ok goal-id)
    )
)

;; Contribute progress towards a group goal
(define-public (contribute-to-goal (goal-id uint) (contribution-amount uint))
    (let (
        (goal-data (unwrap! (map-get? group-goals goal-id) err-goal-not-found))
        (group-id (get group-id goal-data))
        (member-data (unwrap! (map-get? group-members {group-id: group-id, member: tx-sender}) err-not-member))
        (contrib-key {goal-id: goal-id, contributor: tx-sender})
        (existing-contrib (default-to 
            {contribution-value: u0, last-updated: u0, milestones-achieved: u0, has-claimed-reward: false}
            (map-get? goal-contributions contrib-key)
        ))
        (current-block stacks-block-height)
        (new-total-progress (+ (get current-progress goal-data) contribution-amount))
    )
        (asserts! (get is-active goal-data) err-goal-not-active)
        (asserts! (get is-active member-data) err-not-member)
        (asserts! (< current-block (get deadline goal-data)) err-invalid-deadline)
        (asserts! (> contribution-amount u0) err-insufficient-progress)
        
        ;; Update goal progress
        (let ((goal-completed (>= new-total-progress (get target-value goal-data))))
            (map-set group-goals goal-id
                (merge goal-data {
                    current-progress: new-total-progress,
                    is-completed: goal-completed,
                    completion-date: (if goal-completed (some current-block) none)
                })
            )
            
            ;; Update contributor's contribution
            (map-set goal-contributions contrib-key
                (merge existing-contrib {
                    contribution-value: (+ (get contribution-value existing-contrib) contribution-amount),
                    last-updated: current-block
                })
            )
            
            ;; Update member's contribution score
            (map-set group-members {group-id: group-id, member: tx-sender}
                (merge member-data {
                    contribution-score: (+ (get contribution-score member-data) contribution-amount)
                })
            )
            
            ;; If goal completed, update group stats
            (if goal-completed
                (begin
                    (unwrap-panic (update-group-completion-stats group-id))
                    (ok "Goal completed!")
                )
                (ok "Progress updated!")
            )
        )
    )
)

;; Submit peer review for group member
(define-public (submit-peer-review (group-id uint) (reviewee principal) (review-type (string-ascii 30)) 
                                   (rating uint) (feedback (string-ascii 300)))
    (let (
        (review-key {reviewer: tx-sender, reviewee: reviewee, group-id: group-id})
        (group-data (unwrap! (map-get? learning-groups group-id) err-group-not-found))
        (reviewer-data (unwrap! (map-get? group-members {group-id: group-id, member: tx-sender}) err-not-member))
        (reviewee-data (unwrap! (map-get? group-members {group-id: group-id, member: reviewee}) err-not-member))
        (current-block stacks-block-height)
    )
        (asserts! (not (is-eq tx-sender reviewee)) err-cannot-review-self)
        (asserts! (get is-active group-data) err-group-not-found)
        (asserts! (get is-active reviewer-data) err-not-member)
        (asserts! (get is-active reviewee-data) err-not-member)
        (asserts! (and (>= rating u1) (<= rating u5)) err-peer-review-exists)
        (asserts! (is-none (map-get? peer-reviews review-key)) err-peer-review-exists)
        
        ;; Create peer review
        (map-set peer-reviews review-key
            {
                review-type: review-type,
                rating: rating,
                feedback: feedback,
                created-at: current-block,
                is-verified: false,
                reward-claimed: false
            }
        )
        
        ;; Update reviewer stats
        (map-set group-members {group-id: group-id, member: tx-sender}
            (merge reviewer-data {
                peer-reviews-given: (+ (get peer-reviews-given reviewer-data) u1)
            })
        )
        
        ;; Update reviewee stats
        (map-set group-members {group-id: group-id, member: reviewee}
            (merge reviewee-data {
                peer-reviews-received: (+ (get peer-reviews-received reviewee-data) u1)
            })
        )
        
        ;; Reward both parties
        (unwrap-panic (distribute-peer-review-rewards tx-sender reviewee))
        (ok true)
    )
)

;; Leave a learning group
(define-public (leave-group (group-id uint))
    (let (
        (group-data (unwrap! (map-get? learning-groups group-id) err-group-not-found))
        (member-data (unwrap! (map-get? group-members {group-id: group-id, member: tx-sender}) err-not-member))
    )
        (asserts! (get is-active member-data) err-not-member)
        (asserts! (not (is-eq tx-sender (get creator group-data))) err-unauthorized) ;; Creator cannot leave
        
        ;; Mark member as inactive
        (map-set group-members {group-id: group-id, member: tx-sender}
            (merge member-data {is-active: false})
        )
        
        ;; Update group member count
        (map-set learning-groups group-id
            (merge group-data {
                current-members: (- (get current-members group-data) u1)
            })
        )
        
        (ok true)
    )
)

;; Private helper function to update group completion stats
(define-private (update-group-completion-stats (group-id uint))
    (let (
        (group-data (unwrap! (map-get? learning-groups group-id) err-group-not-found))
        (leaderboard-data (unwrap! (map-get? group-leaderboard group-id) err-group-not-found))
        (experience-gained u100)
        (new-total-goals (+ (get total-goals-completed group-data) u1))
        (new-experience (+ (get group-experience-points group-data) experience-gained))
        (new-level (calculate-group-level new-experience))
    )
        ;; Update group stats
        (map-set learning-groups group-id
            (merge group-data {
                total-goals-completed: new-total-goals,
                group-experience-points: new-experience,
                group-level: new-level
            })
        )
        
        ;; Update leaderboard
        (map-set group-leaderboard group-id
            (merge leaderboard-data {
                goals-completed: new-total-goals,
                total-score: new-experience
            })
        )
        
        (ok true)
    )
)

;; Private helper to calculate group level based on experience
(define-private (calculate-group-level (experience uint))
    (if (>= experience u1000)
        u5
        (if (>= experience u750)
            u4
            (if (>= experience u500)
                u3
                (if (>= experience u250)
                    u2
                    u1
                )
            )
        )
    )
)

;; Private helper to distribute peer review rewards
(define-private (distribute-peer-review-rewards (reviewer principal) (reviewee principal))
    (let (
        (reward-amount peer-review-reward)
        (platform-pool (var-get platform-rewards-pool))
    )
        (if (>= platform-pool (* reward-amount u2))
            (begin
                (try! (as-contract (stx-transfer? reward-amount tx-sender reviewer)))
                (try! (as-contract (stx-transfer? reward-amount tx-sender reviewee)))
                (var-set platform-rewards-pool (- platform-pool (* reward-amount u2)))
                (ok true)
            )
            (ok false) ;; Not enough in reward pool
        )
    )
)

;; Read-only functions
(define-read-only (get-learning-group (group-id uint))
    (map-get? learning-groups group-id)
)

(define-read-only (get-group-member (group-id uint) (member principal))
    (map-get? group-members {group-id: group-id, member: member})
)

(define-read-only (get-group-goal (goal-id uint))
    (map-get? group-goals goal-id)
)

(define-read-only (get-goal-contribution (goal-id uint) (contributor principal))
    (map-get? goal-contributions {goal-id: goal-id, contributor: contributor})
)

(define-read-only (get-peer-review (reviewer principal) (reviewee principal) (group-id uint))
    (map-get? peer-reviews {reviewer: reviewer, reviewee: reviewee, group-id: group-id})
)

(define-read-only (get-group-leaderboard (group-id uint))
    (map-get? group-leaderboard group-id)
)

(define-read-only (get-member-activity (member principal) (group-id uint))
    (map-get? member-activity {member: member, group-id: group-id})
)

(define-read-only (is-group-member (group-id uint) (member principal))
    (match (map-get? group-members {group-id: group-id, member: member})
        member-data (get is-active member-data)
        false
    )
)

(define-read-only (get-next-group-id)
    (var-get next-group-id)
)

(define-read-only (get-next-goal-id)
    (var-get next-goal-id)
)

(define-read-only (get-platform-rewards-pool)
    (var-get platform-rewards-pool)
)

(define-read-only (calculate-member-reputation (group-id uint) (member principal))
    (let (
        (member-data (map-get? group-members {group-id: group-id, member: member}))
        (activity-data (map-get? member-activity {member: member, group-id: group-id}))
    )
        (match member-data
            member-info
                (let (
                    (contribution-score (get contribution-score member-info))
                    (reviews-given (get peer-reviews-given member-info))
                    (reviews-received (get peer-reviews-received member-info))
                    (goals-contributed (get goals-contributed member-info))
                )
                    (+ contribution-score 
                       (* reviews-given u10)
                       (* reviews-received u5)
                       (* goals-contributed u25)
                    )
                )
            u0
        )
    )
)

(define-read-only (get-group-stats (group-id uint))
    (let (
        (group-data (map-get? learning-groups group-id))
        (leaderboard-data (map-get? group-leaderboard group-id))
    )
        (match group-data
            group-info
                (some {
                    group: group-info,
                    leaderboard: leaderboard-data,
                    is-active: (get is-active group-info)
                })
            none
        )
    )
)
