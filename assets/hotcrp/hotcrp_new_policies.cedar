// 1. PC Chairs can do anything with papers, reviews and meta-reviews as long as they are not an author of that paper
permit (principal, action, resource is Paper)
when {principal.isPCChair && !(principal in resource.authors)};

permit (principal, action, resource is Review)
when {principal.isPCChair && !(principal in resource.ofPaper.authors)};

// 2. Area Chairs can read, delete, update reviews for any paper as long as they are not an author of that paper
permit (principal, action, resource is Paper)
when {principal has areaChair && !(principal in resource.authors)};

// 3. Area Chairs can read, delete, update meta-reviews for any paper as long as they are not an author of that paper
permit (principal, action in [Action::"Read", Action::"Update", Action::"Delete"], resource is Review)
when {principal has areaChair && !(principal in resource.ofPaper.authors)};

// 4. Area Chairs can make any review or meta-review visible to authors for any paper as long as they are not an author of that paper
permit (principal, action in Action::"ReleaseReview", resource is Review)
when {principal has areaChair && !(principal in resource.ofPaper.authors)};

// 5. Reviewers can read reviews for any paper as long as they are not an author of that paper  
permit (principal, action in Action::"Read", resource is Review)
when {principal in resource.ofPaper.reviewers && !(principal in resource.ofPaper.authors)};

// 7. Reviewers can only update reviews of the papers they are assigned  
permit (principal, action in Action::"Update", resource is Review)
when {principal in resource.ofPaper.reviewers};

// 8. Authors can read, update, delete papers they have written
permit (principal, action, resource is Paper)
when {principal in resource.authors};

// 9. Authors can read reviews and meta-reviews of papers they are authors of
permit (principal, action in Action::"Read", resource is Review)
when {principal in resource.ofPaper.authors};
