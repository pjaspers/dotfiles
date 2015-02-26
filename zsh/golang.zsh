if (( $+commands[go] )) ;
then
    # Development in go path
    export GOPATH=~/development/go
    export PATH=$PATH:$GOROOT/bin

    # Add local tools to bin
    export PATH=$PATH:$GOPATH/bin
fi
