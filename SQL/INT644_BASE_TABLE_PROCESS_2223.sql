--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- REQUIRED TABLES
-- 1. DALL_REF.POSTCODE_LOOKUP 
-- 2. NHSJOBS2_20230407.VACANCY_MASTER
-- 3. NHSJOBS2_202304007.JOBTYPE_CODES 
-- 4. ADNSH.NHS_SALARY_BANDS (SELF GENERATED DATA)
-- 5. ADNSH.BSA_DDAT_ROLES

-- OBJECTIVES
-- 1. ADD VACANY REGION INFORMATION
-- 2. ADD BSA VACANCY TECH FLAG
-- 3. ADD BSA VACANCY HARD TO FILL ROLE FLAG
-- 4. IMPUTE SALARY MEAN FROM EITHER BAND_CD OR SALARY TEXT INFO
-- 5. IMPUTE BAND FROM SALARY TEXT OR VACANCY-MASTER SALARY INFORMATION
-- 6. SELECT AND PROCESS REQUIRED VAACANCY-MASTER DATA

--------------------------------------------------------------------------------
-- Set up common variables that are used throughout the script

--select the tables that contains the jobs metadata 
define jobs_table = 'nhsjobs2_20230407.vacancy_master' 
define jobs_table_codes = 'nhsjobs2_20230407.jobtype_codes' 

--Define the financial year of interest and the month range of interest
define f_year = '22/23'
define year_range = '(202204, 202205, 202206, 202207, 202208, 202209, 202210, 202211, 202212, 202301, 202302, 202303)'

-- INT644: BASE DATA CREATION AND FEATURE GENERATION ---------------------------
create table int644_base_data_2223 compress for query high as

with

-- PART ONE: GENERATE BSA-LEVEL TECH FLAG --------------------------------------

jobs as (
select
    vac_ref,
    job_title, 
    length(job_title) - length(replace(job_title, ' ', '')) + 1   as count_words
from
    (
    select
        vac_ref,
        trim(regexp_replace(regexp_replace(upper(job_title), '[^[:alnum:]]', ' '), '( ){2,}', ' '))   as  job_title
    from 
        &jobs_table

    )
)
--select * from jobs;
,

split_jobs as (
select
    vac_ref,
    job_title,
    substr(jb.job_title, 1, instr(jb.job_title, ' ', 1, t.SPACE_POSITION) -1)   as tokens_one,
    substr(jb.job_title, instr(jb.job_title, ' ', 1, t.SPACE_POSITION) +1)   as tokens_two
from
    jobs  jb,
    (select level as SPACE_POSITION from dual connect by level <= (select max(count_words) from jobs))    t
where       
    1=1
    and t.SPACE_POSITION < jb.count_words
)
--select * from split_jobs order by vac_ref;
,

tokens as (
select
    /*+ materialize */
    vac_ref,
    job_title,
    trim(regexp_substr(job_title,'[^ ]+', 1, lines.column_value))  as  tokens
from
    jobs,
    table(cast(multiset
    (select level from dual connect by instr(job_title, ' ', 1, LEVEL - 1) > 0)  as  sys.odciNumberList))  lines

union 

select vac_ref, job_title, job_title as tokens from jobs

union 

select vac_ref, job_title, tokens_one as tokens from split_jobs

union 

select vac_ref, job_title, tokens_two as tokens from split_jobs
)
--select * from tokens order by vac_ref;
,

flags as (
select
    vac_ref,
    job_title,
    tokens,
    -- ONE: TECH WORDS ---------------------------------------------------------
    case when tokens in 
    (
    'AGILE', 
    'ANALYST',
    'APPLICATION',
    'ARCHITECT',
    'CLOUD',
    'CONTENT',
    'CYBER', 
    'DATA',
    'DATA SCIENTIST',
    'DELIVERY MANAGER', 
    'DELIVERY',
    'DESIGNER',
    'DEVELOPER',
    'DEVOPS',
    'DIGITAL', 
    'FRONT END',
    'FRONTEND',
    'INFORMATION GOVERNANCE',
    'IT', 
    'JAVA',
    'NETWORK', 
    'PRODUCT MANAGER',
    'PYTHON', 
    'SCRUM', 
    'SECURITY',
    'SERVER', 
    'SERVICE OWNER',
    'SOLUTION',
    'STATISTICAL',
    'STATISTICAL', 
    'SYSTEM',
    'TECHNICAL SERVICE', 
    'TECHNOLOGY',
    'UR',
    'USER RESEARCHER',
    'USER RESEARCH',
    'UX',
    'WAREHOUSE', 
    'WEBOPS'
    ) then 1 else 0 end as tech_words,
  -- TWO: NON-TECH WORDS NORMAL ------------------------------------------------
  case when tokens in
    ( 
    '111', 
    'ACCESS', 
    'ACCOUNT ADMINISTRATOR',
    'ACCOUNTANT', 
    'ACUTE', 
    'ADMIN ANALYST', 
    'ADMIN OFFICER',
    'ADMIN', 
    'ADMIN', 
    'ADMINISTRATION',
    'ADMINISTRATION', 
    'ADMINISTRATIVE', 
    'ADMINISTRATOR', 
    'ADVOCATE', 
    'AMBULANCE', 
    'ANAESTHESIA', 
    'ANALYST MANAGER',
    'ANALYST NORTH',
    'ANALYST SOUTH',
    'ANALYST WEST', 
    'APPLICATION ASSISTANT',
    'APPLICATION COORDINAT',
    'APPLICATION OFFICER',
    'APPLICATION SPECIALIST', 
    'APPRAISAL',
    'ARMED FORCES', 
    'ASSESSMENT ADMIN',
    'ASSESSOR', 
    'ASSURANCE', 
    'ATTENDANT', 
    'AUDITOR', 
    'AUXILIARY',
    'BACK OFFICE', 
    'BAND 2', 
    'BAND 3',
    'BEHAVIOUR ANALYST', 
    'BEHAVIOUR CHANGE', 
    'BETTER START',
    'BILLING', 
    'BIOANALYST', 
    'BIOLOGIST', 
    'BLOOD',
    'BLOOD', 
    'BOOKING',
    'BREAST', 
    'BUILDING', 
    'BUSINESS ANALYST', 
    'BUSINESS CHANGE MANAGER',
    'CALL CENTRE', 
    'CALL COORDINAT',
    'CAMHS', 
    'CAMPAIGNS',
    'CANCER', 
    'CAR PARK',
    'CAR PARK OFFICER', 
    'CARDIAC', 
    'CARDIOLOGY', 
    'CARDIOTHORACIC', 
    'CASEWORK', 
    'CATERING',
    'CHARITY',
    'CHEF', 
    'CHEMIST', 
    'CHILD', 
    'CHILDREN', 
    'CHILDRENS', 
    'CLEANER', 
    'CLERICAL',
    'CLERK', 
    'CLINICAL',
    'CLINICIAN', 
    'COLORECTAL', 
    'COLPOSCOPY', 
    'COMMUNITY',
    'COMPLIANCE',
    'COMPLIANTS',
    'CONGENITAL', 
    'CONSULTANT',
    'CONTACT CENTRE', 
    'CONTENT ADVISOR',
    'CONTENT EDITOR',
    'CONTENT OFFFICER',
    'CONTRACT ANALYST', 
    'CONTRACT', 
    'CONTRACTING',
    'COST', 
    'COSTING',
    'CRAFTSPERSON',
    'CRIME', 
    'CRISIS', 
    'CRITICAL CARE',
    'CURRICULUM', 
    'CV',
    'DATA ADMINISTRATOR', 
    'DATA COLLECTION', 
    'DATA COLLECTOR', 
    'DATA COORDINAT', 
    'DATA ENTRY', 
    'DATA INPUT', 
    'DATA QUALITY OFFICER',
    'DATA RECORDER', 
    'DATA SUMARISER', 
    'DATA VALIDATOR', 
    'DECONTAMINATION',
    'DENTAL', 
    'DESKSIDE', 
    'DIABETES',
    'DICTIONARY', 
    'DIETICIAN', 
    'DIETITIAN', 
    'DIRECTOR STRATEGY',
    'DISCHARGE', 
    'DOCTOR',
    'DOMESTIC', 
    'DONATION', 
    'DRIVER', 
    'EARLY YEARS',
    'ECONOMIC ANALYST', 
    'EDUCATION', 
    'ELECTRICAL',
    'ELECTRICAL', 
    'EMERGENCY CARE', 
    'ENDOMETRIOSIS', 
    'ENDOSCOPY', 
    'EPIDEMIOLOG', 
    'ESCALATION', 
    'ESTATES', 
    'EVIDENCE ANALYST',
    'FACILITIES MANAGEMENT',
    'FACILITIES MANAGER',
    'FACILITIES MANAGER', 
    'FIBROSIS',
    'FINANCE ANALYST', 
    'FINANCE OFFICER', 
    'FINANCIAL', 
    'FIRE', 
    'FORENSIC', 
    'FUNDRAISING', 
    'FUNDRAISING', 
    'GENERIC APPLICATION',
    'GENETIC',
    'GLAUCOMA', 
    'GOVERNANCE', 
    'GP', 
    'HAEMATOLOGY',
    'HAEAMOGLOBIN', 
    'HAEMOGLOBIN', 
    'HAEMOPHILA', 
    'HAEMOPHILIA', 
    'HEALTH CARE',
    'HEALTH', 
    'HEALTHCARE',
    'HEART FAILURE', 
    'HEPATOLOGY', 
    'HOUSEKEEPER',
    'HR',
    'HUMAN RESOURCES',
    'INFECTION', 
    'INSTALLATION', 
    'INTEGRATED CARE', 
    'INTENSIVE CARE',
    'INTRADAY ANALYST', 
    'INVENTORY', 
    'JANITOR',
    'JAUNDICE', 
    'JUNIOR CONTENT', 
    'LANGUAGE', 
    'LAUNDRY', 
    'LEARNING ADMINISTRATOR',
    'LIAISON', 
    'LIASION', 
    'LIVER', 
    'LOGISTICS',
    'LUNG', 
    'MAINTENANCE', 
    'MAMMOGRAPHER',
    'MATERNITY',
    'MATRON',
    'MEASUREMENT ANALYST',
    'MECHANICAL',
    'MEDICAL',
    'MEDICINE', 
    'MEDICINES', 
    'MENTAL HEALTH',
    'MICROBIOL', 
    'MICROBIOLOGY', 
    'MIDWIFE',
    'MIDWIVES', 
    'MORTUARY', 
    'MULTISYSTEMIC',
    'NEIGHBOURHOOD',
    'NEONATAL', 
    'NEURO',
    'NEURO',
    'NICE', 
    'NIGHT', 
    'NURSE',
    'NURSING',
    'OBSERVER', 
    'OCCUPATIONAL',
    'OFF LINE', 
    'ONCOLOGY', 
    'OPERATIVE',
    'OPTHALMIC', 
    'OPTHALMOLOGY', 
    'ORTHOPAEDIC', 
    'OUTCOMES MANAGER',
    'OUTPATIENTS', 
    'OUTREACH', 
    'PA ADMIN', 
    'PA', 
    'PAEDIATRIC',
    'PAEDRIATRICS',
    'PALLIATIVE',
    'PARAMEDIC',
    'PARKING', 
    'PATHOLOGY', 
    'PATHOLOGY', 
    'PATHWAY COORDINATOR', 
    'PATIENT', 
    'PAYMENT OFFICER', 
    'PAYROLL ANALYST', 
    'PEOPLE INFORMATION',
    'PEOPLE SYSTEMS ANALYST', 
    'PERFORMANCE', 
    'PERSON', 
    'PHARMACIST',
    'PHARMACY',
    'PHLEBOTOMIST',
    'PHOTOPHERESIS', 
    'PHYSIOLOGIST', 
    'PHYSIOTHERAPIST',
    'PLANNING ANALYST',
    'PLASTER',
    'PLUMBING', 
    'PMO', 
    'PODIATRIST',
    'POLICY', 
    'PORTER',
    'PORTFOLIO',
    'PRACTICE', 
    'PRACTICIONER',
    'PRACTITIONER',
    'PRESCRIBING ANALYST',
    'PRIMARY CARE',
    'PRINTER', 
    'PRIOR APPROVAL ANALYST', 
    'PRIVACY OFFICER', 
    'PROCUREMENT', 
    'PRODUCTION OPERATIVE',
    'PROJECT MANAGER',
    'PRROJECT PLANNING',
    'PSYCHOLOGICAL',
    'PSYCHOLOGIST',
    'PURCHASE ASSISTANT',
    'QC', 
    'QUALITY ASSISSTANT', 
    'QUALITY IMPROVEMENT',
    'QUALITY MANAGER',
    'RADIOGRAPHER',
    'RADIOLOGY',
    'REAL TIME',
    'RECEPTION',
    'RECEPTIONIST',
    'RECRUITMENT',
    'REGISTRAR', 
    'REGULATION', 
    'REHABILITATION', 
    'RENAL', 
    'RESEARCH OFFICER',
    'RESEARCH OFFICER', 
    'RESOLUTION ADMIN',
    'RESOURCING ANALYST', 
    'RESPIRATORY', 
    'RETAIL', 
    'RETIRE',
    'ROSTER ANALYST',
    'ROSTERING', 
    'ROTA', 
    'SAFEGUARDING',
    'SAFETY', 
    'SALARIED GP',
    'SCHEDULE', 
    'SCHEDULING',
    'SCREENING', 
    'SECRETARY', 
    'SECURITY ASSISTANT',
    'SECURITY COORDINATOR', 
    'SECURITY MANAGEMENT', 
    'SECURITY OFFICER', 
    'SECURITY SUPERVISOR', 
    'SERVICE DESK', 
    'SESSIONAL WAREHOUSE',
    'SHOP', 
    'SISTER',
    'SITE',
    'SOCIAL SERVICES',
    'SONOGRAPHER',
    'SPECULATIVE', 
    'SPINAL', 
    'SSYSTEMS ASSISTANT',
    'STAFF APPLICATION',
    'STAFFING ASSISTANT',
    'STAFFING OFFICER', 
    'STERILE', 
    'STROKE', 
    'SUBSTANCE MISUSE', 
    'SUMMARISER',
    'SUPPLY CHAIN ANALYST', 
    'SUPPORT',
    'SURGERY', 
    'SURGICAL', 
    'SYSTEM WIDE COORDINATOR', 
    'SYSTEMATIC REVIEWER', 
    'SYSTEMS ADMINISTRATOR', 
    'TAX', 
    'THEATRE', 
    'THERAPIST',
    'THERAPY',
    'TISSUE', 
    'TRAINER', 
    'TRAINING', 
    'TRAUMA', 
    'TRIAGE',
    'TRIAL', 
    'TUMOUR', 
    'TUTOR', 
    'ULREASOUND',
    'URGENT CARE',
    'VALIDATION', 
    'VASCULAR', 
    'VEHICLE', 
    'VENUE',
    'VERY HIGH INTENISTY', 
    'VIOLENCE', 
    'VIROLOGY', 
    'VOLUNTEER', 
    'WAITING LIST', 
    'WARD',
    'WAREHOUSE OPERATIVE',
    'WAREHOUSE STOR', 
    'WATER', 
    'WELLBEING',
    'WELSH',
    'WHEELCHAIR', 
    'WORKER', 
    'WORKSHOP', 
    'YOUNG', 
    'YOUTH'
    ) then 1 
    -- THREE: NON-TECH WORDS REGEX (1/5)----------------------------------------
    when regexp_like (
    tokens,
    'ADMIN.*ANALYST| 
    ADMIN.*OFFICER|
    ANALYST.*NORTH|
    ANALYST.*WEST| 
    APPLICATION.*ASSISTANT|
    APPLICATION.*COORDINAT|
    APPLICATION.*OFFICER|
    APPLICATION.*SPECIALIST| 
    ARMED.*FORCES| 
    ASSESSMENT.*ADMIN'
    ) then 1
    -- FOUR: NON-TECH WORDS REGEX (2/5)----------------------------------------
    when regexp_like (
    tokens,
    'BEHAVIOUR.*CHANGE| 
    CALL.*COORDINAT|
    CLAIMS.*OFFICER|
    CONTENT.*ADVISOR|
    CONTENT.*EDITOR|,
    CONTENT.*OFFFICER| 
    COURSE.*LEAD| 
    DATA.*COORDINAT| 
    DATA.*VALIDATOR| 
    DELIVERY.*IMPROVEMENT'
    ) then 1
    -- FIVE: NON-TECH WORDS REGEX (3/5)----------------------------------------
    when regexp_like(
    tokens,
    'DIRECTOR.*STRATEGY|
    EVIDENCE.*ANALYST|
    FACILITIES.*MANAGEMENT|
    FACILITIES.*MANAGER| 
    FINANCE.*ANALYST| 
    FINANCE.*OFFICER| 
    HUMAN.*RESOURCES|
    INTENSIVE.*CARE|
    JUNIOR.*CONTENT|
    LEARNING.*ADMINISTRATOR'
    ) then 1
    -- SIX: NON-TECH WORDS REGEX (3/5)------------------------------------------
    when regexp_like(
    tokens,
    'OUTCOMES.*MANAGER|
    PA.*ADMIN| 
    PATHWAY.*COORDINATOR| 
    PAYMENT.*OFFICER| 
    PAYROLL.*ANALYST| 
    PRESCRIBING.*ANALYST| 
    PRIVACY.*OFFICER| 
    PRROJECT.*PLANNING|
    PURCHAS.*ASSISTANT|
    RESEARCH.*OFFICER'
    ) then 1
    -- SEVEN: NON-TECH WORDS REGEX (3/5)----------------------------------------
    when regexp_like(
    tokens,
    'RESEARCH.*OFFICER| 
    RESOLUTION.*ADMIN|
    SALARIED.*GP| 
    STAFF.*APPLICATION| 
    STAFFING.*ASSISTANT|
    STAFFING.*OFFICER| 
    SUBSTANCE.*MISUSE| 
    SUPPLY CHAIN.*ANALYST| 
    WAREHOUSE.*STOR'
    ) then 1 else 0 end as non_tech_words,
  -- FINAL TECH WORDS ----------------------------------------------------------
  case when tokens in 
    (
    'APPLICATION DEVELOPER',
    'DATABASE',
    'DATA SCIENTIST',
    'DATA SECURITY',
    'DATA WAREHOUSE',
    'DIGITAL PROJECT',
    'JAVA',
    'PYTHON',
    'SOFTWARE'
    ) then 1 
  -- FINAL COMPLETE JOB TITLES -------------------------------------------------
  when job_title in (select job_title from ADNSH.int638_bsa_ddat_roles) then 1 else 0 end as final_tech_words
from
    tokens
)
--select * from flags;
,

agg_flag as (
select
    vac_ref,
    job_title,
    case 
        when sum(tech_words) = 0 then 0
        else round(sum(tech_words) / (length(job_title) - length(replace(job_title, ' ', '')) + 1), 4)
    end as  prop_tech_words,
    max(tech_words)        as  tech_words,
    max(non_tech_words)    as  non_tech_words,
    max(final_tech_words)  as  final_tech_words
from
    flags
group by
    vac_ref,
    job_title
)
--select * from agg_flag;
,

bsa_tech_flag as (
select
    vac_ref,
    job_title  as  cleaned_job_title,
    tech_words,
    non_tech_words,
    final_tech_words,
    prop_tech_words,
    case
        when tech_words = 1 and non_tech_words = 0 then 1
        when final_tech_words = 1 then 1
        when prop_tech_words > 0.5 then 1
        else 0
    end as tech_flag
from
    agg_flag
where
    case
        when tech_words = 1 and non_tech_words = 0 then 1
        when final_tech_words = 1 then 1
        when prop_tech_words > 0.5 then 1
        else 0
    end = 1
)
--select * from bsa_tech_flag;
,

-- PART TWO: GENERATE BSA-LEVEL HARD TO FILL ROLE FLAG -------------------------

hard_role_flag as (
select
    btf.vac_ref,
    btf.tech_flag,
    case 
        when 
           cleaned_job_title like '%ARCHITECT%'
        or cleaned_job_title like '%AUTOMATION%'
        or cleaned_job_title like '%USER EXPERIENCE%'
        or cleaned_job_title like '%PLATFORM%'
        or cleaned_job_title like '%DEVELOPER%'
        or cleaned_job_title like '%INTERACTION DESIGNER%'
        or cleaned_job_title like '%SERVICE DESIGNER%'
        or cleaned_job_title like '%SOLUTION DESIGNER%'
        or cleaned_job_title like '%SOLUTIONS DESIGNER%'
        or cleaned_job_title like '%TEST ANALYST%'
        or cleaned_job_title like '%USER EXPERIENCE%'
    then 1 else 0 end as hard_role_flag
from
    bsa_tech_flag  btf
)
--select * from hard_role_flag;
,

-- PART THREE: GENERATE REGION INFORMATION FOR ALL VACANCIES -------------------

postcode_info as (
-- 1. SUBSTRING FIRST 1 CHAR -------------------
select
    postcode,
    sha_code,
    sha_name,
    country_code,
    country_name
from
    (
    select
        distinct
        substr(pcd_no_spaces,1,1)  as  postcode,
        sha_code,
        sha_name,
        country_code,
        country_name,
        count(distinct sha_name) over (partition by substr(pcd_no_spaces,1,1)) as region_count
    from 
        dall_ref.postcode_lookup
    where
        sha_name is not null
    )
where
    region_count = 1

union
-- 2. SUBSTRING FIRST 2 CHARS -------------------
select
    postcode,
    sha_code,
    sha_name,
    country_code,
    country_name
from
    (
    select
        distinct
        substr(pcd_no_spaces,1,2)  as  postcode,
        sha_code,
        sha_name,
        country_code,
        country_name,
        count(distinct sha_name) over (partition by substr(pcd_no_spaces,1,2)) as region_count
    from 
        dall_ref.postcode_lookup
    where
        sha_name is not null
    )
where
    region_count = 1

union
-- 3. SUBSTRING FIRST 3 CHARS -------------------
select
    postcode,
    sha_code,
    sha_name,
    country_code,
    country_name
from
    (
    select
        distinct
        substr(pcd_no_spaces,1,3)  as  postcode,
        sha_code,
        sha_name,
        country_code,
        country_name,
        count(distinct sha_name) over (partition by substr(pcd_no_spaces,1,3)) as region_count
    from 
        dall_ref.postcode_lookup
    where
        sha_name is not null
    )
where
    region_count = 1

union
-- 4. SUBSTRING FIRST 4 CHARS --------------------
select
    postcode,
    sha_code,
    sha_name,
    country_code,
    country_name
from
    (
    select
        distinct
        substr(pcd_no_spaces,1,4)  as  postcode,
        sha_code,
        sha_name,
        country_code,
        country_name,
        count(distinct sha_name) over (partition by substr(pcd_no_spaces,1,4)) as region_count
    from 
        dall_ref.postcode_lookup
    where
        sha_name is not null
    )
where
    region_count = 1

union
-- 5. FULL POSTCODE INFO -------------------------
select
    distinct
    replace(pcd_no_spaces, ' ', '')  as  postcode,
    sha_code,
    sha_name,
    country_code,
    country_name
from
    dall_ref.postcode_lookup
where
    sha_name is not null
)
--select * from postcode_info;
,

-- PART FOUR: IMPUTE SALARY AND BAND FROM OTHER MULTIPLE OTHER FIELDS ----------

salary_edit as (
select
    vac_ref,
    trim(regexp_replace(regexp_replace(replace(replace(salary, '.', ' '), ',', ''), '([0-9]{5,})?.', '\1 '), '( ){2,}', ' ')) as salary_edit
from
   &jobs_table
)
--select * from salary_edit;
,

salary_token as (
select
    /*+ materialize */
    vac_ref,
    trim(regexp_substr(salary_edit,'[^ ]+', 1, lines.column_value))  as  salary_token
from
    salary_edit,
    table(cast(multiset
    (select level from dual connect by instr(salary_edit, ' ', 1, LEVEL - 1) > 0)  as  sys.odciNumberList))  lines
    order by vac_ref
)
--select * from salary_token;
,

salary_impute as (
select
    vac_ref,
    floor(avg(salary_token))  as  salary_mean_from_text
from 
    (
    select
        /*+ materialize */
        vac_ref,
        trim(regexp_substr(salary_edit,'[^ ]+', 1, lines.column_value))  as  salary_token
    from
        salary_edit,
        table(cast(multiset
        (select level from dual connect by instr(salary_edit, ' ', 1, LEVEL - 1) > 0)  as  sys.odciNumberList))  lines
        order by vac_ref
    )
group by
    vac_ref
)
--select * from salary_impute;
,

salary_text_band_impute as (
select
    vac_ref,
    salary_mean_from_text,
    sb.band_cd  as  band_from_salary_text
from
    salary_impute  si
cross join
    (select band_cd, lower_salary, upper_salary_edit from adnsh.nhs_salary_bands where fiscal_year = &f_year)  sb
where
    salary_mean_from_text >= lower_salary
    and salary_mean_from_text < upper_salary_edit
)
--select * from salary_band_impute;
,

salary_value_band_impute as (
select
    vac_ref,
    sb.band_cd  as  band_from_salary_value
from
    &jobs_table
cross join
    (select band_cd, lower_salary, upper_salary_edit from adnsh.nhs_salary_bands where fiscal_year = &f_year)  sb
where
    salary_match_type >= lower_salary
    and salary_annual_high < upper_salary_edit
)
--select * from salary_value_band_impute;
,

band_impute as (
select
    bd.vac_ref,
    (sb.lower_salary + sb.upper_salary)/2  as  salary_mean_from_band
from
    &jobs_table  bd
left join
    (select * from adnsh.nhs_salary_bands where fiscal_year = &f_year)  sb
    on sb.band_cd  =  bd.band_cd
)
--select * from band_impute;
,

salary_mean as (
select
    vac_ref,
    (salary_match_type + salary_annual_high)/2  as  salary_mean_from_value
from
    &jobs_table
where
    (salary_match_type + salary_annual_high)/2 >= (select lower_salary from adnsh.nhs_salary_bands where band_cd = '1' and fiscal_year = &f_year)
)
--select count(*) from salary_mean;
,

-- PART FIVE: JOIN ONTO VACANCY_MASTER FOR INITIAL COALESCE --------------------

vacm  as (
select
    -- VACANCY_MASTER -----------------------
    vm.vac_ref,
    vm.job_title,
    trim(regexp_replace(regexp_replace(upper(vm.job_title), '[^[:alnum:]]', ' '), '( ){2,}', ' '))   as  cleaned_job_title,
    vm.location,
    trim(regexp_replace(upper(replace(vm.postcode, ' ', '')), '[^[:alnum:]]', ''))   as  postcode,
    -- JOB TYPE DESC ------------------------
    jc.jobtype_desc,
    --- POSTCODE_LOOKUP ---------------------
    pl.country_code,
    pl.country_name,
    pl.sha_code  as  region_code,
    pl.sha_name  as  region_name,
    -- VACANCY_MASTER -----------------------
    to_number(to_char(create_date, 'YYYYMM'))  as  year_month,
    create_date,
    expiry_date,
    ceil(expiry_date - create_date)            as  days_open,
    display_ref,
    client_job_ref,
    created_by,
    case when created_by = 'NHS Jobs 3' then regexp_substr(direct_apply_url, '([^\/]+$)') else null end as jobs3_reference,
    direct_apply_url,
    vm.vac_dscrpn,
    vacancy_agency_name,
    case when vacancy_agency_name = 'NHS Business Services Authority' then 1 else 0 end as bsa_flag,
    -- TECH AND HARD ROLE FLAGS -------------
    nvl(hr.tech_flag, 0)       as  tech_flag,
    nvl(hr.hard_role_flag, 0)  as  hard_role_flag,
    -- SALARY FROM VACANCY_MASTER -----------
    salary,
    salary_match_type              as  salary_annual_low,
    salary_annual_high,
    -- SALARY IMPUTED INFO ------------------
    sm.salary_mean_from_value,
    stbi.salary_mean_from_text,
    bi.salary_mean_from_band,
    coalesce(sm.salary_mean_from_value, stbi.salary_mean_from_text, bi.salary_mean_from_band)  as  salary_mean,
    case 
        when sm.salary_mean_from_value is null and 
        coalesce(sm.salary_mean_from_value, stbi.salary_mean_from_text, bi.salary_mean_from_band) is not null
        then 1 else 0 
    end as salary_imputed_flag,
    -- BAND IMPUTED INFO --------------------
    band_cd  as  band_cd_value,
    svbi.band_from_salary_value,
    stbi.band_from_salary_text,
    coalesce(band_cd, svbi.band_from_salary_value, stbi.band_from_salary_text)  as  band_cd,
    case 
        when band_cd is null and 
        coalesce(band_cd, svbi.band_from_salary_value, stbi.band_from_salary_text) is not null
        then 1 else 0 
    end as band_imputed_flag
from
    &jobs_table  vm
left join
    postcode_info   pl
    on pl.postcode  =  replace(vm.postcode, ' ', '')
left join
    hard_role_flag  hr
    on hr.vac_ref  =  vm.vac_ref
left join
    salary_text_band_impute  stbi
    on stbi.vac_ref  =  vm.vac_ref
left join
    salary_value_band_impute  svbi
    on svbi.vac_ref  =  vm.vac_ref
left join
    salary_mean  sm
    on sm.vac_ref  =  vm.vac_ref
left join
    band_impute  bi
    on bi.vac_ref  =  vm.vac_ref
left join
    &jobs_table_codes  jc
    on jc.jobtype  =  vm.jobtype
where
    1=1
    and to_number(to_char(vm.create_date, 'YYYYMM')) in &year_range
)
--select * from vacm

select
    vac_ref,
    vc.job_title,
    cleaned_job_title,
    vc.location,
    postcode,
    -- JOB TYPE DESC ------------------------
    vc.jobtype_desc,
    --- POSTCODE_LOOKUP ---------------------
    region_code,
    region_name,
    -- VACANCY_MASTER -----------------------
    year_month,
    create_date,
    expiry_date,
    days_open,
    display_ref,
    client_job_ref,
    created_by,
    vc.jobs3_reference,
    jr.jobs3_vacancy_id,
    direct_apply_url,
    vac_dscrpn,
    -- ONLY KEY ORG VAC_DSCRPN -------------------------------------------------
    case
        when vc.vacancy_agency_name in (
        'NHS Business Services Authority',
        'South Tyneside and Sunderland NHS Foundation Trust',
        'NHS Digital',
        'NHS England and NHS Improvement',
        'The Newcastle upon Tyne Hospitals NHS Foundation Trust',
        'North of England Commissioning Support',
        'Cumbria, Northumberland, Tyne and Wear NHS Foundation Trust'
        ) then vac_dscrpn
    else null
    end as vac_dscrpn_key,
    vc.vacancy_agency_name,
    -- KEY ORG FLAG ------------------------------------------------------------
    case
        when vc.vacancy_agency_name = 'NHS Business Services Authority' then 'NHSBSA'
        when vc.vacancy_agency_name in (
        'South Tyneside and Sunderland NHS Foundation Trust',
        'NHS Digital',
        'NHS England and NHS Improvement',
        'The Newcastle upon Tyne Hospitals NHS Foundation Trust',
        'North of England Commissioning Support',
        'Cumbria, Northumberland, Tyne and Wear NHS Foundation Trust'
        ) then 'Key NHS Organisation'
    else 'Other NHS Organisation'
    end as org_type,
    bsa_flag,
    -- TECH AND HARD ROLE FLAGS -------------
    tech_flag,
    hard_role_flag,
    -- SALARY FROM VACANCY_MASTER -----------
    salary,
    salary_annual_low,
    salary_annual_high,
    -- SALARY IMPUTED INFO ------------------
    salary_mean_from_value,
    salary_mean_from_text,
    salary_mean_from_band,
    -- PART SEVEN: COALESCE NEW FIGURES FROM INITIAL BASE TABLE ----------------
    salary_mean  as  salary_mean_one,
    case 
        when coalesce(salary_mean, jb.salary_mean_jobtype_value) <= 200000 
        then coalesce(salary_mean, jb.salary_mean_jobtype_value)
        else null
    end  as  salary_mean,
    case 
        when salary_mean_from_value is null and 
        coalesce(salary_mean, jb.salary_mean_jobtype_value) is not null
        then 1 else salary_imputed_flag 
    end as salary_imputed_flag,
    -- BAND IMPUTED INFO --------------------
    band_cd_value,
    band_from_salary_value,
    band_from_salary_text,
    band_cd  as  band_cd_one,
    coalesce(band_cd, jb.band_cd_jobtype_value)  as  band_cd,
    case 
        when band_cd is null and 
        coalesce(band_cd, jb.band_cd_jobtype_value) is not null
        then 1 else band_imputed_flag 
    end as band_imputed_flag
from 
    vacm  vc
left join
    adnsh.int638_jobs3_reference  jr
    on jr.jobs3_reference  =  vc.jobs3_reference
left join
    (
    -- PART SIX: GENERATE NEW IMPUTATIONS FROM INITIAL BASE TABLE --------------
    select
        job_title, 
        jobtype_desc, 
        location, 
        vacancy_agency_name, 
        median(salary_mean)  as  salary_mean_jobtype_value,
        max(band_cd)         as  band_cd_jobtype_value
    from
        vacm
    group by
        job_title, 
        jobtype_desc, 
        location, 
        vacancy_agency_name
        
    )  jb
    on jb.job_title             =  vc.job_title
    and jb.jobtype_desc         =  vc.jobtype_desc
    and jb.location             =  vc.location
    and jb.vacancy_agency_name  =  vc.vacancy_agency_name
-- GLOBAL FILTERS 
where
    1=1
    and year_month in &year_range
    and country_name = 'England';