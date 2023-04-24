# Project Proposal

## Story Pitch
Traditionally, almost everyone feels that investing money in education is worth it, as educational resources can create significant potential wealth for the future. Nowadays, the rising cost of college tuition in the United States has become a major concern for students and their families, as many struggles to keep up with the ever-increasing prices. More and more individuals are concerned about whether paying more for a college education actually leads to higher post-college salaries in the future. This is the question at the core of our data project and research, which aims to analyze the relationship between college tuition costs and post-college earnings. Using data provided from our datasets, the project explores how factors such as geographic location, college degree type, and potential salary impact the cost of tuition and the earning potential of graduates. By delving deep into the data and uncovering the underlying patterns, the project seeks to shed new light on the complex relationship between education and income in the U.S. Rising tuition costs have left many students burdened with significant debt, while also limiting access to higher education for those from lower-income backgrounds. It is not unusual that individuals after graduation will need to work for a few years to finish paying their college tuition debt. By using real-world data we seek to provide a more nuanced understanding of how college costs impact post-college financial outcomes. Our research can be useful and interesting for students and families trying to make informed decisions about where to invest their education money, as well as for policymakers seeking to understand the address of issues of equity and access in higher education. Moreover, as the cost of higher education continues to rise and concerns about student debt grow, this data project has the potential to shed new light on the broader societal implications of rising tuition costs. The project’s findings could inform discussions about the role of government, industry, and individuals in ensuring that higher education remains accessible and affordable for all who seek it. The data on post-college salaries provides valuable insights into how salaries vary by college type, major, and region. It highlights the vast disparities in earnings that exist between different types of colleges and regions, which may have important implications for students as they choose where to attend college. Meanwhile, the data on college tuition costs, diversity, and pay contained in the tuition_cost.csv file comes from a  variety of sources, including the US Department of Education, the Chronicle of Higher Education, and TuitionTracker.org. This dataset is especially useful because it provides detailed information on college tuition costs broken down by geographic location, degree type, and income bracket. By examining how tuition costs vary across different groups of individuals, this dataset can help us better understand the impact of rising tuition costs on students and families.
Overall, our project and research will focus on two datasets to explore the relationship between college tuition costs and post-college salaries. By examining how these variables interact, and visualizing the relationship between the college tuition cost and potential income of graduates for multiple different groups of individuals, we can gain a more comprehensive understanding of the value of higher education and the challenges faced by students and families in paying for it, and solve the question that a lot of individuals are concerned about. The project's findings could have significant implications for policymakers, educators, and individuals seeking to make informed decisions about their education and career paths.

## Finding Data
### Where did you download the data?
We downloaded our data from following URLs:
[Where it Pays to Attend College](https://www.kaggle.com/datasets/wsj/college-salaries) - salaries-by-region.csv
[College tuition, diversity, and pay](https://www.kaggle.com/datasets/jessemostipak/college-tuition-diversity-and-pay) | Kaggle- tuition_cost.csv

### How was the data collected or generated?
For the dataset of Where it Pays to Attend College, it was obtained from the Wall Street Journal based on data from Payscale, Inc:
[Salaries for Colleges by Region](http://online.wsj.com/public/resources/documents/info-Salaries_for_Colleges_by_Region-sort.html)
The data is about the salaries for graduates based on different types of colleges, college majors, and college locations in different regions. And the salaries-by-region.csv we selected is about the different regions of colleges and their income of the graduation students. 

For the dataset of College tuition, diversity, and pay, the data comes from many different sources but originally came from the US Department of Education. 
Tuition and fees by college/university for 2018-2019, along with school type, degree length, state, in-state vs out-of-state from the [Chronicle of Higher Education](https://www.chronicle.com/interactives/tuition-and-fees).
Diversity by college/university for 2014, along with school type, degree length, state, in-state vs out-of-state from the [Chronicle of Higher Education](https://www.chronicle.com/interactives/student-diversity-2016).
Example diversity graphics from [Priceonomics](https://priceonomics.com/ranking-the-most-and-least-diverse-colleges-in/).
Average net cost by income bracket from [TuitionTracker.org](https://www.tuitiontracker.org/).
Example price trend and graduation rates from [TuitionTracker.org](https://www.tuitiontracker.org/school.html?unitid=228778)
The data was downloaded and cleaned by Thomas Mock for #TidyTuesday during the week of March 10th, 2020. It is provided in the TidyTuesday repository and is licensed under the [MIT License](https://github.com/rfordatascience/tidytuesday/blob/master/LICENSE). The data is about the costs of college tuition in the US, by geographic location/area, college degree type, and the potential salary. And the tuition_cost.csv we selected contains multiple colleges information and their tuition cost for different groups of individuals.

### How many observations (rows) are in your data?
For salaries-by-region.csv from Where it Pays to Attend College, it has 320 unique values(rows).
For tuition_cost.csv from College tuition, diversity, and pay, it has 2938 unique values(rows).

### How many features (columns) are in the data?
For salaries-by-region.csv from Where it Pays to Attend College, it has 8 features(columns).
For tuition_cost.csv from College tuition, diversity, and pay, it has 10 features(columns).


## Background Research/Inspiration
- https://www.cnbc.com/2021/11/02/the-gap-in-college-costs-and-earnings-for-young-workers-since-1980.html
  - This article focuses on how college tuition costs have increased dramatically over time, while the percentage of jobs that require postsecondary education have also increased a lot. 
  - The main difference between this article and our project is that this article 
doesn’t dive into the colleges and college types that allow students to earn more or less, as well as what factors affect those earnings. 
- https://www.forbes.com/sites/michaeltnietzel/2022/08/30/how-forbes-top-25-colleges-stack-up-on-alumni-salaries/?sh=292238285fff
  - This article dives into the top colleges for alumni salaries, focusing on concepts such as the trajectory of salaries, and taking a specific look at which colleges lead to high and low salaries is definitely an aspect that will be essential to our project. 
  - A key difference however is that this article mainly focuses on the top 25 colleges, while we will be factoring into account a lot more colleges in order to gain a deeper understanding. 
- https://www.ssa.gov/policy/docs/research-summaries/education-earnings.html
  - This article focuses mostly on how getting a college degree leads to higher median lifetime earnings, even after controlling for factors that can impact whether or not someone completes college, and how overall even with inflation and the value of the dollar changing it is very helpful to get a college degree earning wise, which will definitely be an aspect of this project. 
  - This article differs because we will focus more on which specific factors actually lead to those higher median lifetime earnings, so in a sense we will be diving deeper into the concepts covered by this article instead of glossing over it at a surface level. 
- https://www.forbes.com/advisor/student-loans/is-college-worth-it/
  - This article dives into reasons to consider paying for college and analyzes the pros and cons of getting a college degree, as well as alternatives to getting one. 
  - On the other hand, our project aims to analyze the financial pros and cons of paying for college and what factors can make it financially worth it. 
- https://www.indeed.com/career-advice/pay-salary/average-salary-in-us
  - This article focuses on average salary in the United states and includes a lot of demographic factors that affect that, which is very similar to our project as those are some major factors that we will be including in our analysis. 
  - Our project specifically aims to connect information about those factors that affect salary and tie them into the dataset of the cost of tuition and see how those factors impact the two. 


