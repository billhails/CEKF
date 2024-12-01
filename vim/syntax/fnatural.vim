if exists("b:current_syntax")
    finish
endif

syn match fnDelimeter "(\|)\|\[\|\]\|,\|;\|{\|}"
highlight link fnDelimeter Delimeter

syntax keyword fnFunction print error rand car cdr putc fputc getc fgetc putn fputn putv fputv puts fputs gets fgets open openmem close assert argv opendir closedir readdir ftype getenv com_real com_imag com_mag com_theta error
highlight link fnFunction Function

syntax keyword fnConditional if else then back
highlight link fnConditional Conditional

syntax keyword fnKeyword let fn in typedef here link as unsafe namespace switch alias macro infix prefix suffix
highlight link fnKeyword Keyword

syntax match fnOperator "\v\*\*"
syntax match fnOperator "\v\*"
syntax match fnOperator "\v×"
syntax match fnOperator "\v÷"
syntax match fnOperator "\v/"
syntax match fnOperator "\v\+"
syntax match fnOperator "\v-"
syntax match fnOperator "\v\%"
syntax match fnOperator "\v\@\@"
syntax match fnOperator "\v\@"
syntax match fnOperator "\v\<\=\>"
syntax match fnOperator "\v\<\="
syntax match fnOperator "\v\>\="
syntax match fnOperator "\v\=\="
syntax match fnOperator "\v\!\="
syntax match fnOperator "\v\!"
syntax match fnOperator "\v\|"
syntax match fnOperator "\v\<"
syntax match fnOperator "\v\>"
syntax match fnOperator "\v\="
syntax keyword fnOperator and nand or nor not xor xnor of
highlight link fnOperator Operator

syntax region fnString start=/\v'/ skip=/\v\\./ end=/\v'/
syntax region fnString start=/\v"/ skip=/\v\\./ end=/\v"/
highlight link fnString String

syntax match fnComment "\v\/\/.*$"
highlight link fnComment Comment

syntax match fnConstant "\v\[\]"

syntax keyword fnConstant true false nil nothing some lt eq gt failure success
syntax keyword fnConstant basic_null basic_number basic_string basic_char io_read
syntax keyword fnConstant io_write io_append left right cons _
syntax keyword fnConstant ftype_socket ftype_symlink ftype_regular ftype_block ftype_dir ftype_char ftype_fifo
syntax keyword fnConstant GC_Ll GC_Lm GC_Lo GC_Lt GC_Lu GC_Mc GC_Me GC_Mn GC_Nd GC_Nl GC_No GC_Pc GC_Pd GC_Pe GC_Pf GC_Pi GC_Po GC_Ps GC_Sc GC_Sk GC_Sm GC_So GC_Zl GC_Zp GC_Zs GC_Cc GC_Cf GC_Co GC_Cs GC_Cn 


highlight link fnConstant Constant

syntax keyword fnType bool number char string list cmp try maybe basic_type io_mode ftype_type unicode_general_category_type

highlight link fnType Type

let b:current_syntax = "fnatural"
