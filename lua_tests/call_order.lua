function fa(v)
    __print('a')
    __print(v)
    return 'a'
end

function fb(v)
    __print('b')
    __print(v)
    return 'b'
end

function fc()
    fd = fa
    __print('c')
    return 'c'
end

function fe(v1, v2)
    __print('e')
    __print(v1)
    __print(v2)
end

-- fd(fc()) -- attempt to call global 'fd' (a nil value)

fe(fc(), fd('x'))