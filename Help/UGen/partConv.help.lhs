> Sound.SC3.UGen.Help.viewSC3Help "PartConv"
> Sound.SC3.UGen.DB.ugenSummary "PartConv"

> import Sound.SC3

> let { fft_size = 2048
>     ; ir_file = "/home/rohan/data/audio/reverbs/chapel.wav"
>     ; ir_length = 62494 {- frame count of ir_file -}
>     ; accum_size = pc_calcAccumSize fft_size ir_length
>     ; ir_td_b = 10 {- time domain -}
>     ; ir_fd_b = 11 {- frequency domain -}
>     ; target_b = 12 {- source signal -}
>     ; target_file = "/home/rohan/data/audio/pf-c5.snd"
>     ; c = constant
>     ; g = let { i = playBuf 1 AR (c target_b) 1 0 0 Loop DoNothing
>               ; pc = partConv i (c fft_size) (c ir_fd_b) }
>           in out 0 (pc * 0.1) }
> in withSC3 (do
>     {_ <- async (b_allocRead ir_td_b ir_file 0 ir_length)
>     ;_ <- async (b_alloc ir_fd_b accum_size 1)
>     ;send (pc_preparePartConv ir_fd_b ir_td_b fft_size)
>     ;_ <- async (b_allocRead target_b target_file 0 0)
>     ;play g })
