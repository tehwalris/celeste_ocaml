function fa(v)
    print('a', v)
    return 'a'
end

function fb(v)
    print('b', v)
    return 'b'
end

function fc()
    fd = fa
    print('c')
    return 'c'
end

function fe(v1, v2)
    print('e', v1, v2)
end

-- fd(fc()) -- attempt to call global 'fd' (a nil value)

fe(fc(), fd('x')) -- prints "e c a"