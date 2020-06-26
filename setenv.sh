cslib_path="$HOME"

add_to_colon_list() {
    local name="$1"
    local new_value="$2"
    local value="${!name}"
    if [ -z "$value" ] ; then
        export "$name"="$new_value"
    else
        IFS=':' read -a vs <<< "$value"
        local value=''
        for v in "${vs[@]}" ; do
            if [ "$new_value" != "$v" ] ; then
                value+=:"$v"
            fi
        done
        export "$name"="$new_value""$value"
    fi
}

export CHEZSCHEMELIBDIRS=".:$cslib_path"
add_to_colon_list LD_LIBRARY_PATH "$cslib_path/cslib/clib"
