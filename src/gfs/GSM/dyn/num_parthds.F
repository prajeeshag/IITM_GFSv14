      function num_parthds()
      integer:: number_of_openMP_threads
      character(2) :: omp_threads
      integer :: stat
      call get_environment_variable("OMP_NUM_THREADS",omp_threads)
      read(omp_threads,*,iostat=stat)number_of_openMP_threads
      num_parthds = number_of_openMP_threads
        !num_parthds = omp_get_num_threads()
      return
      end 

