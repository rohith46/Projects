#cust_dimen market_fact orders_dimen prod_dimen shipping_dimen 

select sum(sales) as 'Total Sales', avg(sales) as 'Average Sales'
from market_fact;

select region,count(*) as No_of_Customers
from cust_dimen
group by region
order by No_of_Customers desc;

limit 1;

select prod_id as 'product id' , count(*) as no_of_products_sold
from market_fact
group by prod_id
order by no_of_products_sold desc ;

select C.customer_name as 'Customer Name', sum(M.order_quantity) as no_of_tables
from market_fact M inner join cust_dimen C on M.cust_id = C.cust_id
         inner join prod_dimen P on P.prod_id = M.prod_id
where C.region = 'atlantic' and P.product_sub_category = 'tables'
group by C.customer_name
order by no_of_tables desc;

select P.Product_Category, sum(M.profit) as profits
from market_fact M inner join prod_dimen P on M.prod_id = P.prod_id
group by P.Product_Category
order by profits desc;

2nd way 
select P.Product_Category, sum(M.profit) as profits
from market_fact M, prod_dimen P 
where M.prod_id = P.prod_id
group by P.Product_Category
order by profits desc;

select P.Product_Category, P.Product_Sub_Category, sum(M.profit) as profits
from market_fact M inner join prod_dimen P on M.prod_id = P.prod_id
group by P.Product_Category,P.Product_Sub_Category
order by profits desc;


select P.Product_Sub_Category, C.region, count(*) as no_of_shipments, sum(M.profit) as profits
from market_fact M inner join prod_dimen P on M.prod_id = P.prod_id
         inner join cust_dimen C on C.cust_id = M.cust_id
group by C.region,P.Product_Sub_Category
order by profits asc;

select Region, count(*) as no_of_table_shipments, sum(M.profit) as profits
from market_fact M, prod_dimen P, cust_dimen C 
where M.prod_id = P.prod_id and 
        C.cust_id = M.cust_id and P.product_sub_category = (select P.product_sub_category from prod_dimen where p.product_sub_category = min(M.profit))
group by C.region
order by profits desc;


select Region, count(*) as no_of_table_shipments, sum(M.profit) as profits
from market_fact M, prod_dimen P, cust_dimen C 
where M.prod_id = P.prod_id and 
        C.cust_id = M.cust_id and P.product_sub_category = (select P.product_sub_category from market_fact M, prod_dimen P where P.Prod_id= M.Prod_id
group by P.product_sub_category
order by sum(M.profit) asc
limit 1)
group by C.region
order by profits desc;