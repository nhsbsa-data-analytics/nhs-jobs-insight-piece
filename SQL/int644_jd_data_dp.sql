------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------SCRIPT START----------------------------------------------------------------------------------------------------------------------
create table int644_jd_data_dp compress for query high as 

with
base as
(
select * from int644_base_data_2122
union all 
select * from int644_base_data_2223
)
,
vac_refs as
(
select * from base
where cleaned_job_title like '%DATA SCIENTIST%' OR cleaned_job_title like '%DATA ENGINEER%' OR cleaned_job_title like '%DATA ANALYST%' OR
cleaned_job_title like '%INFORMATION ANALYST%' OR cleaned_job_title like '%PERFORMANCE ANALYST%'
),

vac_refs_filtered as
(
select VAC_REF from vac_refs
where tech_flag=1 OR cleaned_job_title like '%PERFORMANCE ANALYST%'
)

select bd.* from int644_jd_data bd
inner join vac_refs_filtered vr on vr.vac_ref=bd.vac_ref
where file_type in ('PDF','DOCX')
