student_contacts <- fetch_records("Contact")
student_leads <- fetch_records(
    "Lead", 
    add_fields = c(
        "ConvertedContactId", 
        "Invited_to_interview_info_session__c", 
        "Interview_Attendance__c"
    ), 
    force_request = TRUE
)
placements <- fetch_records("TSC_Placement__c")
alumni_surveys <- fetch_records("Alumni_Survey__c")

contact_fields <- fetch_fields("Contact")
student_layout <- fetch_page_layout("Contact", "01250000000MVMMAA4")
applicant_layout <- fetch_page_layout("Lead", "01250000000MVLOAA4")
lead_fields <- fetch_fields("Lead")
 
student_layout |> 
    dplyr::mutate(
        label = section_components |>
            purrr::map(names)
    ) |>
    tidyr::unnest(c(section_components, label)) |>
    dplyr::rename(name = section_components) |>
    View()

student_layout |> 
    dplyr::mutate(
        label = section_components |>
            purrr::map(names)
    ) |>
    tidyr::unnest(c(section_components, label)) |>
    dplyr::rename(name = section_components) |>
    filter(name |>str_detect("ducation"))

lead_fields |>
    dplyr::filter(name |>str_detect("Ontario")) |>
    select(name)



# Fields missing from current student layout
# - Select_Highest_Level_of_Education__c: Highest level of education
# - Highest_Education_Country__c: Country of highest level of education
# - Education__c: Maybe highest level of education? Maybe releated records? 
# - Immigration_and_Citizenship_Status__c: legal immigration status. Maybe incomplete? 


contact_fields |> 
    dplyr::filter(name |>str_detect("Id")) |>
    select(name)

students <- student_contacts |>
    select(
        student_id = Id,
        account_id = AccountId, 
        name = Name,

        # enr: Youth Program Details
        enr__date_of_application = Date_of_Application__c, 
        enr__status = Status_TSC__c,
        enr__branch = Branch__c, 
        enr__stream = Stream__c, 
        enr__date_of_graduation = Graduation_Date__c,
        enr__date_of_bootcamp_start = BootCamp_Start_Date__c, 
        enr__date_of_program_start = Program_Starts_on__c,

        # con: Contact Information
        con__preferred_email = npe01__Preferred_Email__c, 
        con__personal_email = npe01__HomeEmail__c, 
        con__home_phone = HomePhone, 
        con__cell_phone = MobilePhone, 
        con__work_phone = npe01__WorkPhone__c, 

        # dem: Demographic Information
        dem__date_of_birth = Date_of_Birth__c, 
        dem__race_ethnicity = Race_Ethnicity__c, 
        dem__age_at_bootcamp = Age_at_BootCamp_Start_Date__c, 
        dem__gender = Gender__c, 
        dem__year_immigrated = Year_Arrived_in_Canada__c, 
        dem__disability = Disability__c, 
        dem__disability_type = Type_of_Disability_During_Program__c, 

        # fin: Household / Financial information
        fin__housing_type = Housing_Type__c, 
        fin__public_housing = Public_Housing_Resident__c, 
        fin__eviction_risk = At_risk_of_eviction__c, 
        fin__household_income = Household_Income__c, 
        fin__personal_income = Individual_Income__c, 
        fin__n_children = Number_of_Children__c, 
        fin__child_ages = Ages_of_Children_Separated_by_Commas__c, 
        fin__single_parent = Single_Parent__c, 
        fin__income_sources = Monthly_Income_Sources__c, 
        fin__income_amounts = Monthly_Income_Amounts__c, 
        fin__benefits_ow = I_am_receiving_OW__c, 
        fin__benefits_odsp = I_am_receiving_ODSP__c, 

        # abp: Alumni Bridging Program
        abp__networking_date = Cisco_Networking_Essentials_Cloud_Date__c, 
        abp__networking_result = Cisco_Networking_E_Cloud_Result__c, 
        abp__programming_date = Cisco_Programming_E_P_python__c, 
        abp__programming_result = Cisco_Programming_Essentials_in_Python__c, 

        # opt: Immediate Post Training and Current Outcomes
        opt__status = Placement_Status__c, 
        opt__placement_start_date = Secured_Internship_Date__c, 
        opt__placement_end_date = Completed_Internship_Date__c, 
        opt__placement_complete = Completed_Internship__c, 
        opt__placement_employer = Internship_Employer__c, 

        # cur: Current Status
        cur__employment_status = Employment_Post_Training_Status__c, 
        cur__job_search = Actively_Job_Searching__c, 
        cur__education = Enrolled_in_Education__c, 
        cur__education_program = Program_Name__c, 
        cur__hourly_wage = Current_Hourly_Wage__c, 
        cur__salary = Annual_Salary__c,
        cur__job_start_date = Employment_Post_Training_Start_Date__c, 
        cur__job_type = Nature_of_Employment__c
    ) 





applicant_layout |> 
    dplyr::mutate(
        label = section_components |>
            purrr::map(names)
    ) |>
    tidyr::unnest(c(section_components, label)) |>
    dplyr::rename(name = section_components) |>
    View()

applicants <- student_leads  |>
    select(
        applicant_id = Id, 
        student_id = ConvertedContactId, 

        # app: Application Details
        app__application_date = Date_of_Application__c, 
        app__status_01 = Status, 
        app__status_02 = Applicant_status__c, 
        app__reason = Reason__c, 
        app__reason_for_attrition = Reasons_for_Attr__c,
        app__return_applicant = Applied_Participated_Previously__c,
        app__assessment_created = Assessment_created__c,
        app__invited_to_interview = Invited_to_Info_Interview_Session_On__c,  
        app__interview_note_created = Interview_note_created__c,
        app__program_consider = Consider_for_acceptance_into__c, 
        app__program_offer = Program_Offered_in__c, 
        app__bootcamp_offer = Offer_Acceptance_to_Bootcamp__c,
        app__bootcamp_accepted = Boot_Camp_Offer_Accepted__c, 
        app__bootcamp_status = Boot_Camp_Status__c, 
        app__poes_referral = POES_Referral__c,


        # con: Contact Information
        con__lead_email = Email, 
        con__cell_phone = MobilePhone, 
        con__phone = Phone, 

        # dem: Demographic Information
        dem__date_of_birth = DOB__c, 
        dem__race_ethnicity = Race_Ethnicity__c, 
        dem__age_at_bootcamp = Age_at_BootCamp_Start_Date__c, 
        dem__gender = Gender__c, 
        dem__year_immigrated = Year_Arrived_in_Canada__c, 
        dem__disability = Disability__c, 
        dem__disability_type = Type_of_Disability__c, 

        # fin: Financial and Household details
        fin__benefits = Public_Benefits__c, 
        fin__n_children = Number_of_Children__c, 
        fin__personal_income = Individual_Income__c, 
        fin__household_income = Household_Income__c, 
        fin__child_ages = Ages_of_Children_Separated_by_Commas__c, 
        fin__eviction_risk = At_risk_of_eviction__c, 
        fin__public_housing = Public_Housing_Resident__c, 
        fin__housing_type = Housing_Type__c, 
        fin__single_parent = Single_Parent__c

    )

# What tells me an applicant got interviewed? 

## Invited to Interview or Interview Note Created? 

# - Unclear. Some records have interview notes but no invited to interview date
applicants  |>
    count(
        app__poes_referral,
        !is.na(app__invited_to_interview), 
        app__interview_note_created
    )

applicants |>
    count(app__reason)

applicants |>
    count(app__return_applicant)

## Is there some interview field missing from the page layout? 

lead_fields |> filter(name |> str_detect("nterview")) |> select(name)

# Cross-membership applicants / enrollments? 

## How many students aren't represented in the applicants data? 
# - quite a few. 604 to be precise
# - probably going to have to park this clarification for later

applicants |>
    dplyr::mutate(
        has_contact_id = !is.na(student_id),
        in_leads = TRUE
    ) |>
    dplyr::full_join(
        students |>
            mutate(in_contacts = TRUE), 
        by = "student_id"
    ) |>    
    filter(is.na(in_leads), in_contacts) |>
    select(matches("con__|opt__")) |>
    View()

## How many placements do they have? 

names(placements)

job_placements <- placements |>
    select(
        placement_id = Id, 
        student_id = TSC_Student__c,
        
        # job: Job details
        job__benefits = Benefits_Details__c, 
        job__direct_placement = Direct_Placement__c, 
        job__health_benefits = Healthcare_Benefits__c, 
        job__weekly_hours = Hours_Per_Week__c, 
        job__type_01 = Job_Type__c, 
        job__tenure = Employment_Type__c, 
        job__intensity = Employment_Type_II__c,
        job__new_wage_salary = New_Wage_Salary__c, 
        job__wage_salary = Wage_Salary__c, 
        job__wage_updated = Updated_Wage__c, 
        job__tech_related = Tech_Related__c, 
        job__salary_type = Salary_Type__c,
        job__start_date = Start_Date__c, 
        job__end_date = End_Date__c, 

        # rep: reporting details
        rep__excluded_from_retention_calc = Excluded_from_Retention_Calculation__c, 
        rep__excluded_from_retention_followup = Excluded_from_Retention_Follow_Up__c

    )

job_placements |>
    transmute(
        placement_id, 
        student_id, 
        in_placements = TRUE
    ) |>
    full_join( 
        students |>
            transmute(
                student_id, 
                in_contacts = TRUE
            )
    ) |>
    count(in_placements, in_contacts)

# Rolling todo: 
# - pull and recode highest level of education
# - clean benefits
# - oragnize outcomes data
# - coalesce applicant and student fields

# Organize outcomes data
jobs__tsc_placements <- job_placements |>
    transmute(
        student_id, 
        data_source = "A - TSC Placement", 
        employed = TRUE, 
        permanent = job__tenure %in% "Permanent", 
        full_time = job__intensity %in% "Full-Time", 
        annual_earnings = dplyr::case_when(
            job__salary_type %in% "Annual Salary" ~ job__wage_salary, 
            job__salary_type %in% "Hourly" & job__intensity %in% "Full-Time" ~ job__wage_salary * 35 * 50, 
            job__salary_type %in% "Hourly" & !is.na(job__weekly_hours) ~ job__wage_salary * job__weekly_hours * 50, 
            TRUE ~ NA_real_
        ), 
        start_date = job__start_date, 
        end_date = job__end_date
    )

jobs__current <- students |>
    transmute(
        data_source = "B - Contact", 
        employed = !is.na(cur__employment_status), 
        permanent = case_when(
            is.na(cur__employment_status) ~ NA_real_, 
            cur__employment_status |> str_detect("Permanent") ~ TRUE,
            TRUE ~ FALSE
        ),
        full_time = case_when(
            is.na(cur__employment_status) ~ NA_real_, 
            cur__employment_status |> str_detect("Full-Time") ~ TRUE,
            TRUE ~ FALSE
        ),
        in_related_sector = case_when(
            is.na(cur__employment_status) ~ NA_real_, 
            cur__employment_status |> str_detect("In Related") ~ TRUE,
            TRUE ~ FALSE
        ), 
        start_date = cur__job_start_date, 
        end_date = lubridate::as_date(NA)
    )


survey_fields <- fetch_fields("Alumni_Survey__c")

survey_fields |>
    filter(name |> str_detect("[jJ]ob|age")) |>
    select(name)

survey_responses <- alumni_survey_records  |>
    select(

    )


