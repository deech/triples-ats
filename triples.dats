#include "share/atspre_staload.hats"

fun triples(): stream_vt(@(int,int,int)) =
  stream_vt_concat(
    stream_vt_map_cloptr<int><stream_vt(@(int,int,int))>(
      streamize_intrange_l 1,
      lam(i) =<cloptr1>
        let
         val i = i
        in
        stream_vt_concat(
          stream_vt_map_cloptr<int><stream_vt(@(int,int,int))>(
            streamize_intrange_lr(1,i+1),
            lam(j) =<cloptr1>
              let
                val j = j
              in
              stream_vt_map_cloptr<int><(int,int,int)>(
                streamize_intrange_lr(j,i+1),
                lam(k) =<cloptr1> @(i,j,k)
              )
              end
          )
        )
        end
    )
  )

implement main0(argc,argv) =
  stream_vt_con_free(
    stream_vt_foreach(
      stream_vt_takeLte(
        stream_vt_filter_fun(
          triples(),
          lam(ts) =>
            let
              val (i,j,k) = ts
            in
              j*j + k*k = i*i
            end
        )
        ,
        1000
      )) where {
        implement stream_vt_foreach$fwork<@(int,int,int)><void>(x,env) =
          let
            val (i,j,k) = x
          in
            println!(j, "," , k, "," ,i)
          end
      })
