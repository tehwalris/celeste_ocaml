function fa(v)
    print('a')
    print(v)
    return 'a'
end

function fb(v)
    print('b')
    print(v)
    return 'b'
end

function fc()
    fd = fa
    print('c')
    return 'c'
end

function fe(v1, v2)
    print('e')
    print(v1)
    print(v2)
end

-- fd(fc()) -- attempt to call global 'fd' (a nil value)

fe(fc(), fd('x'))