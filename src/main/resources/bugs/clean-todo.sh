for d in `find . -type d -d 1`; do
  echo "$d"
  for todo in `ls $d/TODO`; do
    target=`sed '2q;d' "$d/TODO/$todo"`
    l1=`echo "$target" | xargs`
    for bugs in `ls $d/*.js`; do
      while read line; do
        l2=`echo "$line" | xargs`
        if [ "$l1" == "$l2" ]; then
          echo "$d/TODO/$todo: $l1"
          if [ "$1" == "-d" ]; then
            rm $d/TODO/$todo
          fi
          break
        fi
      done < $bugs
    done
  done
done
