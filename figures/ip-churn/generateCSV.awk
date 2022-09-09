function get_count(from, to) {
    cmd = "./getCount.sh " from " " to
    cmd | getline line
    close(cmd)

    return line
}

function getAddedIP(from1, mid, to2) {
    to1 = mid
    from2 = mid
    countL = get_count(from1, to1)
    countR = get_count(from2, to2)
    countA = get_count(from1, to2)

    return countA - countL
}

function getRemovedIP(from1, mid, to2) {
    to1 = mid
    from2 = mid
    countL = get_count(from1, to1)
    countR = get_count(from2, to2)
    countA = get_count(from1, to2)

    return countA - countR
}

BEGIN{
}
{
    if(NR==1){
        LAST0=$0
    }else if(NR==2){
        LAST1=$0
    }else{
        THIS=$0
        print get_count(LAST0, LAST1), getAddedIP(LAST0, LAST1,THIS), getRemovedIP(LAST0, LAST1,THIS)

        LAST0=LAST1
        LAST1=$0
    }
}
