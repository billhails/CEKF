if exists("b:current_syntax")
    finish
endif

syn match fnDelimeter "(\|)\|\[\|\]\|,\|;\|{\|}"
highlight link fnDelimeter Delimeter

syntax keyword fnFunction print error rand car cdr putc fputc getc fgetc putn fputn putv fputv puts fputs gets fgets open close assert args opendir closedir readdir ftype getenv com_real com_imag com_mag com_theta error
highlight link fnFunction Function

syntax keyword fnConditional if else then back
highlight link fnConditional Conditional

syntax keyword fnKeyword let fn in typedef here link as unsafe namespace switch
highlight link fnKeyword Keyword

syntax keyword FnStatement true false lt eq gt nil cons
highlight link FnStatement Statement

syntax match fnOperator "\v\*\*"
syntax match fnOperator "\v\*"
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
syntax match fnOperator "\v\<"
syntax match fnOperator "\v\>"
syntax match fnOperator "\v\="
syntax keyword fnOperator and nand or nor not xor
highlight link fnOperator Operator

syntax region fnString start=/\v'/ skip=/\v\\./ end=/\v'/
syntax region fnString start=/\v"/ skip=/\v\\./ end=/\v"/
highlight link fnString String

syntax match fnComment "\v\/\/.*$"
highlight link fnComment Comment

syntax keyword fnConstant true false null pair nothing some lt eq gt failure success basic_null basic_number basic_string basic_char io_read io_write io_append

highlight link fnConstant Constant

syntax keyword fnType bool number char string list cmp try maybe basic_type io_mode

highlight link fnType Type

let b:current_syntax = "fnatural"
