module hist_mod

public :: hist_to_graph, stdev, opt_binw 

interface hist_to_graph
  module procedure :: hist_to_graph
end interface hist_to_graph

interface stdev
  module procedure :: stdev
end interface stdev

interface opt_binw
  module procedure :: opt_binw
end interface opt_binw

contains 

! SUBROUTINE TO CONVERT HISTOGRAM TO (X,Y) GRAPH FORMAT
subroutine hist_to_graph(hist,nbins,low,high,graph) 
    implicit none
    
    real,intent(in) :: hist(:),low,high
    integer,intent(in) :: nbins
    real,allocatable,intent(out) :: graph(:,:)
    real :: step,counts
    integer :: i,j,N
    
    allocate(graph(nbins+1,2))
    N = size(hist)
    
    step = (high-low)/nbins
    forall (i=1:nbins+1) graph(i,1) = low + step*(i-1)
    graph(:,2) = 0.0
    !print*,N,nbins,low,high,step
    do i=1,N
       do j=0,nbins
             if ( hist(i) > (graph(j+1,1) - step/2.0) .and. hist(i) < (graph(j+1,1) + step/2.0) ) then
                graph(j+1,2) = graph(j+1,2) + 1.0
                counts = counts + 1.0
             exit
          endif
       enddo
    enddo
    !print*,"counts",counts,sum(graph(:,2))
    return
end subroutine


function stdev(arr) 
    implicit none 
    
    real,intent(in) :: arr(:) 
    real :: stdev,mean
    integer :: N
    
    N = size(arr) 
    mean = sum(arr)/N 
    
    stdev = sum((arr-mean)**2.0)
    stdev = sqrt(stdev/N)
 
    return
 end function stdev


subroutine opt_binw(h,w)
   !use func_subs
   implicit none
   real, intent(in) :: h(:)
   real, intent(out) :: w 
   real, allocatable :: h_diff(:),C(:,:),gr(:,:)
   real :: step,lowlim,uplim,low,high,tempw,temp_mean,temp_std
   integer :: i,N,iter,nbins,indx(1)

   N = size(h) 
   
   allocate(h_diff(N-1))  
   do i=1,N-2 
      h_diff(i) = h(i+1) - h(i)
   enddo 

    
   lowlim = minval(abs(h_diff)) ; uplim = 3.0*stdev(h_diff)!maxval(abs(h_diff))
   step = 0.005*stdev(h_diff)!(uplim-lowlim)/100.0
   iter = nint((uplim-lowlim)/step)
   !print*,lowlim,uplim,step,iter 
   allocate(C(iter,2)) 

   low = minval(h)
   high = maxval(h) 

   do i=1,iter 
      tempw = lowlim + i*step
      nbins = nint((high-low)/tempw)
      call hist_to_graph(h,nbins,low,high,gr)
      temp_mean = sum(gr(:,2))/size(gr(:,2))
      temp_std = stdev(gr(:,2))
      !print*,tempw,nbins,temp_mean,temp_std
      C(i,1) = tempw
      C(i,2) = (2*temp_mean-temp_std**2.0)/(tempw*tempw)
      !print*,i,C(i,:)
      deallocate(gr)
   enddo

   indx = minloc(C(:,2))
   !opt_binw = C(indx(1),1)
   w = C(indx(1),1)
   !print*,indx,w
   deallocate(h_diff,C)
   !close(10)
   return 
end subroutine opt_binw 

end module