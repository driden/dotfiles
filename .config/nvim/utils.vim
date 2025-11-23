function! CsvToKeyValue()
    " Count commas in first line to get column count
    let comma_count = len(substitute(getline(1), '[^,]', '', 'g'))
    let col_count = comma_count + 1
    
    " Split both lines by commas into separate lines
    execute '1s/,/\r/g'
    execute '$s/,/\r/g'
    
    " Go back to top
    normal! gg
    
    " Execute the macro logic in a loop
    let i = 0
    while i < col_count
        normal! yy
        execute 'normal! ' . col_count . 'j'
        normal! P
        normal! J
        execute 'normal! ' . col_count . 'k'
        normal! dd
        let i += 1
    endwhile
endfunction

" Map it to a command
command! CsvToKV call CsvToKeyValue()
