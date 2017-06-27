function project_jump -a name --description 'Jumps to projects'
  set PROJECT_PATHS (ls -d -1 ~/development/*)
  for project in $PROJECT_PATHS
    if test -d $project/$name
      cd $project/$argv
    end
  end
end

function __all_projects --description "Returns all projects"
  ls -d ~/development/*/* | xargs -n 1 basename
end

complete -c project_jump -xa "(__all_projects)"
